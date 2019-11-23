---------------- A PACKAGE -------------------
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

package DBUG_PKG is


constant C_ADDR_DBUG : integer := 13;
constant C_S_ADDR_DBUG : integer := 11;

component cache_debug is
	PORT
	(
		address		: IN STD_LOGIC_VECTOR (7 DOWNTO 0);
		clock		: IN STD_LOGIC  := '1';
		data		: IN STD_LOGIC_VECTOR (299 DOWNTO 0);
		wren		: IN STD_LOGIC ;
		q			: OUT STD_LOGIC_VECTOR (299 DOWNTO 0)
	);
end component;

component debug_memory
	PORT
	(
		address		: IN STD_LOGIC_VECTOR (C_ADDR_DBUG-1 DOWNTO 0);
		clock		: IN STD_LOGIC  := '1';
		data		: IN STD_LOGIC_VECTOR (63 DOWNTO 0);
		wren		: IN STD_LOGIC ;
		q			: OUT STD_LOGIC_VECTOR (63 DOWNTO 0)
	);
end component;

component s_debug_memory
	PORT
	(
		address		: IN STD_LOGIC_VECTOR (C_S_ADDR_DBUG-1 DOWNTO 0);
		clock		: IN STD_LOGIC  := '1';
		data		: IN STD_LOGIC_VECTOR (63 DOWNTO 0);
		wren		: IN STD_LOGIC ;
		q			: OUT STD_LOGIC_VECTOR (63 DOWNTO 0)
	);
end component;

--COMMENTED--component ProbeTree is
--COMMENTED--	port (
--COMMENTED--		source     : out std_logic_vector(83 downto 0);                    -- source
--COMMENTED--		probe      : in  std_logic_vector(83 downto 0) := (others => 'X'); -- probe
--COMMENTED--		source_clk : in  std_logic                     := 'X'              -- clk
--COMMENTED--	);
--COMMENTED--end component ProbeTree;

end package;

---------------- MERKLE TREE PACKAGE -------------------
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;
use IEEE.math_real.all;

library CSHIA;
use CSHIA.handler_pkg.all;

package tree_pkg is

	constant TC_ADDR				: natural := 20;
	constant TC_SETS				: natural := 2; -- Value to force writeback in attack. Original values: 2, 4, 8
	constant TC_LINES				: natural := 4; -- Value to force writeback in attack. Original values: 64, 32, 16
	constant TC_LOG2_LINES			: natural := natural(ceil(log2(real(TC_LINES))));
	constant TC_CHUNK				: natural := 4;
	constant TC_LOG2_CHUNK			: natural := natural(ceil(log2(real(TC_CHUNK))));
--	constant TC_LOG2_CHUNK			: natural := 2;
	constant TC_DEPTH				: natural := TC_ADDR/TC_LOG2_CHUNK;
	constant TC_LOG2_DEPTH			: natural := natural(ceil(log2(real(TC_DEPTH))));
	-- This makes the algorithm either:
	-- STOP updating nodes at the first hit in the tree cache, or
	-- UPDATE all nodes from bottom-top, stoping only after updating the ptag-root.
	constant TC_STOP_WRITE_HIT		: std_logic := '0';
	-- This makes the algorithm either:
	-- STOP bringing nodes to the tree cache at the first hit, or
	-- BRINGS all nodes from the memory to the cache.
	constant TC_STOP_READ_HIT		: std_logic := '1';
	-- This make the algorithm either:
	-- REPLACE any node in the cache, or
	-- TRIES to replace the dirty nodes first.
	constant TC_REPLACE_DIRTY_FIRST : std_logic := '0';

	CONSTANT C_PTAG_LIMIT			: INTEGER := 2**TC_ADDR;
	CONSTANT C_PTAG_ADDR			: INTEGER := TC_ADDR;
	CONSTANT C_PTAG_LINE_SIZE		: INTEGER := TC_CHUNK;
	CONSTANT C_LOG2_PTAG_LINE_SIZE	: INTEGER := TC_LOG2_CHUNK;

	subtype T_PTAG_ADDRESS_STD is std_logic_vector (C_PTAG_ADDR-1 downto 0);

	subtype T_LRU_NATURAL is natural range 0 to 2**(TC_ADDR-TC_LOG2_LINES)-1;
	type T_ARRAY_NATURAL is array (natural range <>) of T_LRU_NATURAL ;
	
	constant C_ALIGN_ADDR			: T_PTAG_ADDRESS_STD := C_DATA_SEG_STARTING_ADDR(C_PTAG_ADDR+C_BASE_ADDR_BITS_W-1 downto C_BASE_ADDR_BITS_W); 

	function find_next_address(addr : T_PTAG_ADDRESS_STD) return std_logic_vector;
	function assemble_address(base_addr : T_PTAG_ADDRESS_STD; level: natural) return std_logic_vector;

	function find_lru4 (lrus : T_ARRAY_NATURAL (3 downto 0)) return natural;
	function find_lru3 (lrus : T_ARRAY_NATURAL (2 downto 0)) return natural;
	function find_lru2 (lru1, lru2: natural) return natural;
	function find_lru (lrus : T_ARRAY_NATURAL(TC_SETS-1 downto 0)) return natural;
end package;

package body TREE_PKG is
	function find_next_address(addr : T_PTAG_ADDRESS_STD)
	return std_logic_vector is

		variable	level			: integer;
		variable	tree_id,
					level_addr,
					tmp_addr,
					next_addr		: T_PTAG_ADDRESS_STD;
	begin
		if addr(C_PTAG_ADDR-1) = '0' then
			tmp_addr	:= std_logic_vector(unsigned(addr) - unsigned(C_ALIGN_ADDR));
		else
			tmp_addr	:= addr;
		end if;
		
		level		:= to_integer(unsigned(addr(C_PTAG_ADDR-2 downto C_PTAG_ADDR-TC_LOG2_DEPTH-1)))+1;
		tree_id		:= std_logic_vector(shift_left(to_unsigned(1, C_PTAG_ADDR), C_PTAG_ADDR-1));
		level_addr	:= std_logic_vector(shift_left(to_unsigned(level, C_PTAG_ADDR), C_PTAG_ADDR-1-TC_LOG2_DEPTH)); 
		next_addr	:= std_logic_vector(shift_right(shift_left(unsigned(tmp_addr), TC_LOG2_DEPTH+1), TC_LOG2_DEPTH+1+TC_LOG2_CHUNK));
		return tree_id or level_addr or next_addr;
	end function;
	function assemble_address(base_addr : T_PTAG_ADDRESS_STD; level: natural)
	return std_logic_vector is

		variable	tree_id,
					level_addr,
					tmp_addr,
					next_addr		: T_PTAG_ADDRESS_STD;
	begin
		tmp_addr	:= std_logic_vector(unsigned(base_addr) - unsigned(C_ALIGN_ADDR));
		tree_id		:= std_logic_vector(shift_left(to_unsigned(1, C_PTAG_ADDR), C_PTAG_ADDR-1));
		level_addr	:= std_logic_vector(shift_left(to_unsigned(level, C_PTAG_ADDR), C_PTAG_ADDR-1-TC_LOG2_DEPTH)); 
		next_addr	:= std_logic_vector(shift_right(shift_left(unsigned(tmp_addr), TC_LOG2_DEPTH+1), TC_LOG2_DEPTH+1+(level*TC_LOG2_CHUNK)));
		return tree_id or level_addr or next_addr;
	end function;


	function find_lru2 (lru1, lru2: natural)
	return natural is
	begin
		if lru1 < lru2 then
			return lru1;
		else
			return lru2;
		end if;
	end function;

	function find_lru3 (lrus : T_ARRAY_NATURAL (2 downto 0))
	return natural is
	begin	
		return find_lru2(lrus(2), find_lru2(lrus(0), lrus(1)));
	end function;

	function find_lru4 (lrus : T_ARRAY_NATURAL (3 downto 0))
	return natural is
		variable	result1, result2		: natural;
	begin	
		result1		:= find_lru2(lrus(0), lrus(1));
		result2		:= find_lru2(lrus(2), lrus(3));
		return find_lru2(result1, result2);
	end function;

	function find_lru (lrus : T_ARRAY_NATURAL(TC_SETS-1 downto 0))
	return natural is
	begin
		case TC_SETS is
		when 2 =>
			return find_lru2(lrus(0), lrus(1));
		when 3 =>
			return find_lru3(lrus);
		when 4 =>
			return find_lru4(lrus);
		when 5 =>
			return find_lru2(lrus(4), find_lru4(lrus(3 downto 0)));
		when 6 =>
			return find_lru2(find_lru3(lrus(2 downto 0)), find_lru3(lrus(5 downto 3)));
		when 7 =>
			return find_lru2(find_lru3(lrus(2 downto 0)), find_lru4(lrus(6 downto 3)));
		when 8 =>
			return find_lru2(find_lru4(lrus(3 downto 0)), find_lru4(lrus(7 downto 4)));
		when others  =>
			report "There is no cache implementation for this configuration" severity failure;
			return 0;
		end case;
	end function;
end package body;
------------------- CACHE PACKAGE ----------------------

LIBRARY ieee;
USE ieee.std_logic_1164.ALL;

LIBRARY CSHIA;
USE CSHIA.tree_pkg.ALL;
USE CSHIA.handler_pkg.ALL;

PACKAGE cache_pkg IS
	CONSTANT C_RAM_WORD			: INTEGER := C_PTAG_WIDTH;
	CONSTANT C_RAM_WIDTH		: INTEGER := C_PTAG_WIDTH*TC_CHUNK;
	CONSTANT C_LOG2_RAM_DEPTH	: INTEGER := TC_LOG2_LINES;
	CONSTANT C_RAM_DEPTH		: INTEGER := 2**C_LOG2_RAM_DEPTH;

	SUBTYPE T_PTAG_WORD IS std_logic_vector (C_RAM_WORD-1 downto 0);
	TYPE T_PTAG_RAM IS ARRAY(C_PTAG_LIMIT-1 downto 0) of T_PTAG_WORD;

	SUBTYPE T_WORD IS std_logic_vector (C_RAM_WIDTH-1 downto 0);
	TYPE T_RAM IS ARRAY(C_RAM_DEPTH-1 downto 0) of T_WORD;

	SUBTYPE T_ADDRESS_VECTOR IS INTEGER RANGE 0 to C_RAM_DEPTH-1;

	type R_STATUS is record
		tag			: std_logic_vector (TC_ADDR-C_LOG2_RAM_DEPTH-TC_LOG2_CHUNK-1 downto 0);
		lru			: natural;
		valid,
		dirty		: std_logic;	
	end record;

	component control IS
		GENERIC (
			N_SETS				: natural range 1 to 32 := 16
		);
		PORT (clk,
			rstn,
			dirtiness,
			incoming_ready,
			ready,
			wrt 				: in	std_logic;
			no_ancestor,
			requesting,
			sending,
			ready_to_receive	: out	std_logic;
			root_level			: in	natural range 0 to TC_DEPTH;
			bus_address			: in	std_logic_vector (TC_ADDR-1 downto 0);
			requested_address	: out	std_logic_vector (TC_ADDR-1 downto 0);
			returning_address	: out	std_logic_vector (TC_ADDR-1 downto 0);
			data_in				: in	std_logic_vector (C_RAM_WIDTH-1 downto 0);
			data_out			: out   std_logic_vector (C_RAM_WIDTH-1 downto 0)
		);
	end component;

	component cache IS
		GENERIC (
			N_SETS				: natural range 1 to 32 := 16
		);
		PORT (clk,
			rstn,
			dirtiness,
			set_invalid,
			verify,
			read,
			wrt					: in	std_logic;
			miss_rw,
			discard,
			hit_rw				: out	std_logic;
			discarding_addr		: out	std_logic_vector (TC_ADDR-1 downto 0);
			address				: in	std_logic_vector (TC_ADDR-1 downto 0);
			data_in				: in	std_logic_vector (C_RAM_WIDTH-1 downto 0);
			data_out			: out   std_logic_vector (C_RAM_WIDTH-1 downto 0)
		);
	end component;

	component cache_set is
		port (
			clock1,
			clock2				: in	std_logic;
			my_number			: in	natural;
			data				: in	T_WORD;
			write_address		: in	T_ADDRESS_VECTOR;
			read_address		: in	T_ADDRESS_VECTOR;
			we					: in	std_logic;
			q					: out	T_WORD
		);
	end component cache_set;

--	component cache_set is
--		port (
--			clk					: in	std_logic;
--			my_number			: in	natural;
--			data				: in	T_WORD;
--			address				: in	T_ADDRESS_VECTOR;
--			we					: in	std_logic;
--			q					: out	T_WORD
--		);
--	end component cache_set;

END PACKAGE cache_pkg;

--------------- FUZZY EXTRACTOR PACKAGE ----------------

----------------- APUF PACKAGE -------------------

library ieee;
use ieee.std_logic_1164.all;

package apuf_pkg is

	CONSTANT C_APUF_CHALLENGE : INTEGER := 64;
	SUBTYPE T_APUF_CHALLENGE IS STD_LOGIC_VECTOR (C_APUF_CHALLENGE-1 DOWNTO 0);
	CONSTANT C_APUF_RESPONSE : INTEGER := 64;
	SUBTYPE T_APUF_RESPONSE IS STD_LOGIC_VECTOR (C_APUF_RESPONSE-1 DOWNTO 0);
	
	component apuf_stage is
		port (
			in1, in2				: in STD_LOGIC;
			path					: in STD_LOGIC_VECTOR (1 downto 0);
			out1, out2				: out STD_LOGIC
		);
	end component;

	component apuf_unit is
		-- The number of stages has to be at least 2 and multiple of 2 .
		generic (
			n_stages 				: integer := 16
		);
		port (
			in1, in2				: in std_logic;
			challenge				: in std_logic_vector (n_stages-1 downto 0);
			out1, out2				: out std_logic
		);
	end component;

	component apuf is
		generic (
			n_stages			: integer := C_APUF_CHALLENGE;
			n_pufs				: integer := C_APUF_RESPONSE
		);
		port (
			clk,
			rst_n,
			clearn				: in std_logic;
			challenge			: in std_logic_vector (n_stages-1 downto 0);
			responses			: out std_logic_vector (n_pufs-1 downto 0)
		);
	end component;
end package;

--------------- MEMORY PACKAGE ----------------
LIBRARY ieee;
USE ieee.std_logic_1164.ALL;

LIBRARY CSHIA;
USE CSHIA.tree_pkg.ALL;
USE CSHIA.handler_pkg.ALL;

package ptag_memory_pkg is
	
	constant	C_REDUX				: INTEGER := 5;
	constant	C_ADDR_BASE			: INTEGER := C_PTAG_ADDR-C_REDUX;
	constant	C_ADDR_TREE			: INTEGER := C_PTAG_ADDR-C_REDUX-2;

	subtype T_PTAG_ADDR_BASE is std_logic_vector(C_ADDR_BASE-1 downto 0);
	subtype T_PTAG_ADDR_TREE is std_logic_vector(C_ADDR_TREE-1 downto 0);

	component ptag_mem_base
		PORT
		(
			address		: IN T_PTAG_ADDR_BASE;
			clock		: IN STD_LOGIC  := '1';
			data		: IN STD_LOGIC_VECTOR (C_PTAG_WIDTH-1 DOWNTO 0);
			wren		: IN STD_LOGIC ;
			q			: OUT STD_LOGIC_VECTOR (C_PTAG_WIDTH-1 DOWNTO 0)
		);
	end component;

	component ptag_mem_tree
		PORT
		(
			address		: IN T_PTAG_ADDR_TREE;
			clock		: IN STD_LOGIC  := '1';
			data		: IN STD_LOGIC_VECTOR (C_PTAG_WIDTH-1 DOWNTO 0);
			wren		: IN STD_LOGIC ;
			q			: OUT STD_LOGIC_VECTOR (C_PTAG_WIDTH-1 DOWNTO 0)
		);
	end component;

end package;


--------------- SRAM PACKAGE ----------------

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

PACKAGE sram_pkg IS
	CONSTANT C_SRAM_ADDR_WIDTH : INTEGER := 6;
	CONSTANT C_SRAM_WORD_LENGTH : INTEGER := 16;
	SUBTYPE T_SRAM_ADDR IS STD_LOGIC_VECTOR (C_SRAM_ADDR_WIDTH-1 DOWNTO 0);
	SUBTYPE T_SRAM_WORD IS STD_LOGIC_VECTOR (C_SRAM_WORD_LENGTH-1 DOWNTO 0);

	COMPONENT sram IS
		port (
			clk           : in    std_logic                     := '0';             --        clock_reset.clk
			reset         : in    std_logic                     := '0';             --  clock_reset_reset.reset
			SRAM_DQ       : inout std_logic_vector(15 downto 0) := (others => '0'); -- external_interface.export
			SRAM_ADDR     : out   std_logic_vector(19 downto 0);                    --                   .export
			SRAM_LB_N     : out   std_logic;                                        --                   .export
			SRAM_UB_N     : out   std_logic;                                        --                   .export
			SRAM_CE_N     : out   std_logic;                                        --                   .export
			SRAM_OE_N     : out   std_logic;                                        --                   .export
			SRAM_WE_N     : out   std_logic;                                        --                   .export
			address       : in    std_logic_vector(19 downto 0) := (others => '0'); --  avalon_sram_slave.address
			byteenable    : in    std_logic_vector(1 downto 0)  := (others => '0'); --                   .byteenable
			read          : in    std_logic                     := '0';             --                   .read
			write         : in    std_logic                     := '0';             --                   .write
			writedata     : in    std_logic_vector(15 downto 0) := (others => '0'); --                   .writedata
			readdata      : out   std_logic_vector(15 downto 0);                    --                   .readdata
			readdatavalid : out   std_logic                                         --                   .readdatavalid
		);
	END COMPONENT;

END PACKAGE;

--------------- BCH PARAM PACKAGE ----------------

LIBRARY ieee;
USE ieee.std_logic_1164.ALL;

PACKAGE bch IS

	CONSTANT KEY_LENGTH : INTEGER := 128;
	CONSTANT RWORD_LENGTH : INTEGER := 64;

	CONSTANT BCH_N : INTEGER := KEY_LENGTH-1;
	CONSTANT BCH_K : INTEGER := RWORD_LENGTH;
	CONSTANT BCH_T : INTEGER := 10;

-- File generated by bch.exe program.
-- The constant package for BCH code (127,64), t=10
-- Option= 3,  Interleave= 1, -- with optimization= 1.
-- GF(2^7) is generated by polynomial [1+x+...] - 11000001;
-- Constant necessary for decoder and encoder

	CONSTANT m: INTEGER:= 7;
	CONSTANT n: INTEGER:= 127;  -- n= 2^m -1  -size of block
	CONSTANT k: INTEGER:= 64;  -- BCH code (n,k) -no. of information bits
	CONSTANT nk: INTEGER:= 63; -- nk=n-k
	CONSTANT t: INTEGER:= 10;  -- no. of errors to be corrected

	CONSTANT sizea: INTEGER:= 4; -- size of counter ca
	CONSTANT sizeb: INTEGER:= 4; -- size of counter cb 
		-- count = iteration* cb + ca
	CONSTANT sizel: INTEGER:= 4; 
		-- size of l integer (degree of error polynomial BMA)

	COMPONENT enc IS
		PORT (clk, reset, din: IN BIT; 
			vdin, dout: OUT BIT); --output serial data
	END COMPONENT;
		-- clk -  to clock encoded data frequency 
		-- clkinf - to clock input information data
		-- din - information data clocked with clkio clock
		-- vdin - valid data in - to enable external data shifting
		-- dout - encoded data to be transmitted 

	COMPONENT dec IS
		PORT (clk, reset, din: IN BIT;
			vdout, dout: OUT BIT);
	END COMPONENT;
		-- clk -  to clock encoded data frequency 
		-- clkinf - to clock input information data
		-- din - information data clocked with clkio clock
		-- dout - encoded data to be transmitted 

END PACKAGE;


--------------- ECC PACKAGE ----------------
LIBRARY ieee;
USE ieee.std_logic_1164.ALL;

LIBRARY CSHIA;
USE CSHIA.bch.ALL;
USE CSHIA.handler_pkg.ALL;
USE CSHIA.apuf_pkg.ALL;

PACKAGE ecc IS

	CONSTANT C_KEY_WORDS : INTEGER := 8;
	CONSTANT C_R_WORDS : INTEGER := 4;

	COMPONENT BCHDecoder IS
		PORT (clk, reset : IN STD_LOGIC;
			codeword : IN STD_LOGIC_VECTOR (RWORD_LENGTH*2-2 DOWNTO 0);
			data : OUT STD_LOGIC_VECTOR (RWORD_LENGTH-1 DOWNTO 0);
			ready : OUT STD_LOGIC
		);
	END COMPONENT;

	COMPONENT BCHEncoder IS
		PORT (clk, reset : IN STD_LOGIC;
			data : IN STD_LOGIC_VECTOR (RWORD_LENGTH-1 DOWNTO 0);
			syndrome: OUT STD_LOGIC_VECTOR (RWORD_LENGTH-2 DOWNTO 0);
			ready : OUT STD_LOGIC
		);
	END COMPONENT;

	component fe_engine is
		PORT (clk,
			rstn,
			reboot					: IN STD_LOGIC;
			helper_data_dec			: IN STD_LOGIC_VECTOR (C_APUF_RESPONSE-1 DOWNTO 0);
			extracted_rword			: IN STD_LOGIC_VECTOR (C_APUF_RESPONSE-1 DOWNTO 0); 
			extracted_key			: IN STD_LOGIC_VECTOR (C_APUF_RESPONSE-1 DOWNTO 0);
			recovered_key			: OUT STD_LOGIC_VECTOR (C_APUF_RESPONSE-1 DOWNTO 0);
			helper_data_enc			: OUT STD_LOGIC_VECTOR (C_APUF_RESPONSE-1 DOWNTO 0);
			done					: OUT STD_LOGIC
		);
	end component;

	component fuzzy_extractor is
		port (clk,
			rstn,
			reboot,
			enroll				: in  std_logic;
			key1				: OUT STD_LOGIC_VECTOR (C_KEY_LENGTH-1 DOWNTO 0);
			key2				: OUT STD_LOGIC_VECTOR (C_KEY_LENGTH-1 DOWNTO 0);
			done				: out std_logic
		);
	end component;

END PACKAGE;

--------------- SIPHASH PACKAGE ----------------

LIBRARY ieee;
USE ieee.std_logic_1164.ALL;

PACKAGE SIPHASh_2_4 IS

	CONSTANT BYTES_WIDTH : INTEGER := 4;
	CONSTANT BLOCK_WIDTH : INTEGER := 2**(BYTES_WIDTH-1)*8;

	CONSTANT V_WIDTH     : INTEGER := BLOCK_WIDTH;
	CONSTANT HASH_WIDTH  : INTEGER := BLOCK_WIDTH;
	CONSTANT KEY_WIDTH   : INTEGER := 2*BLOCK_WIDTH;
	CONSTANT COUNT_WIDTH : INTEGER := 8-(BYTES_WIDTH-1);

	CONSTANT LENGTH_WIDTH : INTEGER := COUNT_WIDTH + BYTES_WIDTH - 1;

	CONSTANT V0_INIT : STD_LOGIC_VECTOR := x"736f6d6570736575";
	CONSTANT V1_INIT : STD_LOGIC_VECTOR := x"646f72616e646f6d";
	CONSTANT V2_INIT : STD_LOGIC_VECTOR := x"6c7967656e657261";
	CONSTANT V3_INIT : STD_LOGIC_VECTOR := x"7465646279746573";

	CONSTANT V2_FINAL : STD_LOGIC_VECTOR := x"00000000000000ff";

	TYPE v_array IS ARRAY (INTEGER RANGE <>) OF STD_LOGIC_VECTOR (V_WIDTH-1 DOWNTO 0);

	COMPONENT sipround IS
		PORT (
			v0_in, v1_in, v2_in, v3_in     : IN  STD_LOGIC_VECTOR (V_WIDTH-1 DOWNTO 0);
			v0_out, v1_out, v2_out, v3_out : OUT STD_LOGIC_VECTOR (V_WIDTH-1 DOWNTO 0)
		);
	END COMPONENT;

	COMPONENT siphash IS
	GENERIC (c : integer := 2);
		PORT (
			m : IN STD_LOGIC_VECTOR (BLOCK_WIDTH-1 DOWNTO 0);
			b : IN STD_LOGIC_VECTOR (BYTES_WIDTH-1 DOWNTO 0);

			rst_n  : IN STD_LOGIC;
			clk    : IN STD_LOGIC;
			init   : IN STD_LOGIC;
--			load_k : IN STD_LOGIC;
			key		: IN STD_LOGIC_VECTOR (KEY_WIDTH-1 downto 0);
	
			init_ready, hash_ready : BUFFER STD_LOGIC;
			hash                   : OUT    STD_LOGIC_VECTOR (HASH_WIDTH-1 DOWNTO 0)
		);
	END COMPONENT;
END PACKAGE;

--------------- PTAG-GEN PACKAGE ----------------

LIBRARY ieee;
USE ieee.std_logic_1164.ALL;

LIBRARY CSHIA;
USE CSHIA.handler_pkg.ALL;

PACKAGE ptag_gen_pkg  IS

	constant	C_TS_ADDR		: integer := 14;
	constant	C_TS_LENGTH		: integer := 16;

	subtype T_TS_WORD is std_logic_vector(C_TS_LENGTH-1 downto 0);
	subtype	T_TS_ADDR is std_logic_vector(C_TS_ADDR-1 downto 0);

	component timestamp
		PORT
		(
			address		: IN T_TS_ADDR;
			clock		: IN STD_LOGIC  := '1';
			data		: IN T_TS_WORD;
			wren		: IN STD_LOGIC ;
			q			: OUT T_TS_WORD 
		);
	end component;

	COMPONENT ptag_generator IS
		PORT (clk,
			rstn					: in std_logic;
			address					: in std_logic_vector (C_ADDR_WIDTH-1 downto 0);
			key						: in std_logic_vector (C_KEY_LENGTH-1 downto 0);
			memory_block			: in std_logic_vector (C_LINE_SIZE*C_WORD_WIDTH-1 downto 0);
			enable_computation		: in std_logic;
--			load_key_done,
			ptag_ready				: out std_logic;
			input_data				: out std_logic_vector (C_SIPHASH_BLOCK_WIDTH-1 downto 0);
			ptag					: out std_logic_vector (C_PTAG_WIDTH-1 downto 0)
		);
	END COMPONENT;	
END PACKAGE;
