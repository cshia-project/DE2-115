library		ieee;
use			ieee.std_logic_1164.all;
use			ieee.numeric_std.all;
-- 
library		grlib;
use			grlib.amba.all;

package handler_pkg is
------------------------------------------------------------------------------
-- All packages global constant declaration 
------------------------------------------------------------------------------
	constant C_ADDR_WIDTH				: natural			:= 32;
	constant C_WORD_WIDTH				: natural			:= 32;

	constant C_MIN_ADDR_RANGE			: std_logic_vector(C_ADDR_WIDTH-1 downto 0)		:= X"40000000";
	constant C_MAX_ADDR_RANGE			: std_logic_vector(C_ADDR_WIDTH-1 downto 0)		:= X"40092FFF"; 
--	constant C_MAX_ADDR_RANGE			: std_logic_vector(C_ADDR_WIDTH-1 downto 0)		:= X"400604FF"; 
--	constant C_MAX_ADDR_RANGE			: std_logic_vector(C_ADDR_WIDTH-1 downto 0)		:= X"40020FFF"; 
	constant C_DATA_SEG_STARTING_ADDR	: std_logic_vector(C_ADDR_WIDTH-1 downto 0)		:= X"40013000";
	constant C_START_PROG				: integer			:= to_integer(unsigned(C_MIN_ADDR_RANGE));
	constant C_START_DATA				: integer			:= to_integer(unsigned(C_DATA_SEG_STARTING_ADDR));
	constant C_FINAL_ADDR				: integer			:= to_integer(unsigned(C_MAX_ADDR_RANGE));

	constant C_BASE_ADDR_BITS			: natural			:= 3;  -- log(linesize) + 2(word size)
	constant C_BASE_ADDR_BITS_W			: natural			:= 3+2;  -- log(linesize) + 2(word size)
	constant C_LINE_SIZE				: natural			:= 8;
	constant C_PTAG_WIDTH				: natural			:= 64; -- 32 for  dummy simulation 64 for real PTAG
	constant C_PTAG_ADDR_WIDTH			: natural			:= 20; 
	constant C_ASSOCIATIVE_BUFF_SIZE	: natural			:= 4;
	constant C_KEY_LENGTH				: natural			:= 128; -- added for simplication
	constant C_SIPHASH_BLOCK_WIDTH		: natural			:= 64; -- added for coherency

	constant C_PTAG_BYTES				: integer			:= 8;
  
	subtype T_ADDRESS is std_logic_vector (C_ADDR_WIDTH-1 downto 0);
	subtype T_PTAG_ADDRESS is std_logic_vector (C_PTAG_ADDR_WIDTH-1 downto 0);
	subtype T_BASE_ADDRESS is std_logic_vector (C_ADDR_WIDTH-C_BASE_ADDR_BITS_W-1 downto 0);
  
	subtype T_PTAG_ADDR_INT is integer range 0 to C_PTAG_ADDR_WIDTH-1;
-------------------------------------------------------------------------------
-- Type decalration
-------------------------------------------------------------------------------
	type line_buffer_type is array (0 to C_LINE_SIZE-1) of std_logic_vector(C_WORD_WIDTH downto 0);
	type associative_buffer is array (0 to C_ASSOCIATIVE_BUFF_SIZE -1 ) of  line_buffer_type;

	type line_buffer_status is record
		address			: T_ADDRESS;
		valid			: std_logic;
		secure			: std_logic;
		dirty			: std_logic;
	end record;
	 
	type associative_buffer_st is array (0 to C_ASSOCIATIVE_BUFF_SIZE -1 ) of  line_buffer_status;

	type ptag_mreq_type is record
		we				: std_logic;
		address			: T_PTAG_ADDRESS;
		data			: std_logic_vector(C_PTAG_WIDTH-1 downto 0);
	end record;

	type ptag_mresp_type is record
		data			: std_logic_vector(C_PTAG_WIDTH-1 downto 0);
	end record;
 
	type ptag_sec_req_type is record
        cache_line		: std_logic_vector((C_LINE_SIZE*C_WORD_WIDTH)-1 downto 0);
        base_addr		: T_ADDRESS;
        valid			: std_logic;
        wr_ptag			: std_logic;
	end record;
   
	type ptag_sec_val_type is record
		ptag			: std_logic_vector(C_PTAG_WIDTH-1 downto 0);
		valid			: std_logic;
		line_secure		: std_logic;
		ready			: std_logic;
		reboot_ready	: std_logic;
	end record;

	component security_engine is
		port (
			clk						: in	std_ulogic;
			hard_rstn				: in	std_ulogic;
			rstn					: in	std_ulogic;
			enroll_done				: in	std_logic;
			enroll_tree				: in	std_logic;
			assemble_done			: out	std_logic;
			status					: out	std_logic_vector(7 downto 0);
-------------------------------------------------------------------------------
			log_out					: out	std_logic_vector(15 downto 0);
-------------------------------------------------------------------------------
			--Ptag memory
			ptag_mreq_out			: out	ptag_mreq_type;
			ptag_mresp_in			: in	ptag_mresp_type;
			--Security interface
			ptag_sreq_in			: in	ptag_sec_req_type;
			ptag_sval_out			: out	ptag_sec_val_type
		);
	end component;

	component bus_handler is
		generic (
			G_LINE_SIZE			: natural := 16;	-- size of the cahche line to be loaded
			G_LINE_WIDTH		: natural := 32);	-- size of the word in bits
		port (
			clk					: in	std_ulogic;
			rstn				: in	std_ulogic;
			bypass_in			: in	std_logic;
			enroll_done			: out	std_logic;
			assemble_done		: in	std_logic;
-------------------------------------------------------------------------------
			log_in				: in	std_logic_vector(15 downto 0);
-------------------------------------------------------------------------------
			ptag_sreq_out		: out	ptag_sec_req_type;
			ptag_sval_in		: in	ptag_sec_val_type;
			--AHB interface
			ahbi_in				: in	ahb_mst_in_type;
			ahbi_out			: out	ahb_mst_in_type;
			ahbo_in				: in	ahb_mst_out_type;
			ahbo_out			: out	ahb_mst_out_type;
			state_out			: out	std_logic_vector(7 downto 0);
			watchdog_en			: in	std_logic	 
			);
	end component;

	component ptag_sram is
	  generic
		(
		  G_RAM_FILE : string  := "ram_init.txt";  --txt file contaning binary data
													--each line
		  DATA_WIDTH : natural := 8;
		  ADDR_WIDTH : natural := 6
		  );
	  port
		(
		  clk  : in  std_logic;
		  addr : in  natural range 0 to 2**ADDR_WIDTH - 1;
		  data : in  std_logic_vector((DATA_WIDTH-1) downto 0);
		  we	: in  std_logic := '1';
		  q	 : out std_logic_vector((DATA_WIDTH -1) downto 0)
		  );

	end component;

	component puf_mem
		PORT
		(
			address		: IN STD_LOGIC_VECTOR (7 DOWNTO 0);
			clock		: IN STD_LOGIC  := '1';
			data		: IN STD_LOGIC_VECTOR (31 DOWNTO 0);
			wren		: IN STD_LOGIC ;
			q			: OUT STD_LOGIC_VECTOR (31 DOWNTO 0)
		);
	end component;

	component ptag_mem
		PORT
		(
			address		: IN T_PTAG_ADDRESS;
			clock		: IN STD_LOGIC  := '1';
			data		: IN STD_LOGIC_VECTOR (C_PTAG_WIDTH-1 DOWNTO 0);
			wren		: IN STD_LOGIC ;
			q		: OUT STD_LOGIC_VECTOR (C_PTAG_WIDTH-1 DOWNTO 0)
		);
	end component;

end package handler_pkg;
