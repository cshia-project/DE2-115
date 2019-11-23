------------------------------------------------------------------------------
-- Title      : Security Engine of CSHIA 
-- Project    : CSHIA
-------------------------------------------------------------------------------
-- File       : security_engine.vhd
-- Author     : Caio 
-- Company    : 
-- Created    : 2017-
-- Last update: 2019-11-23 
-- Platform   : 
-- Standard   : VHDL'93/02
-------------------------------------------------------------------------------
-- Description: this block receives a chache line and checks if this line is
-- compatible with the given hardware
-------------------------------------------------------------------------------
-- Copyright (c) 2019
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author  Description
-- 2017-	   2.0      Caio	Created
-------------------------------------------------------------------------------
--TODO LIST
-- Optimization of Area and Clock cycles
-- Removal of Debug Signals
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library grlib;
use grlib.amba.all;
-- use work.handler_pkg.all;

library CSHIA;
use CSHIA.SipHash_2_4.all;
use CSHIA.ptag_gen_pkg.all;
use CSHIA.handler_pkg.all;
use CSHIA.ecc.all;
use	CSHIA.tree_pkg.all;
use	CSHIA.cache_pkg.all;
use CSHIA.DBUG_PKG.all;

entity security_engine is
	port (
		clk						: in	std_ulogic;
		rstn					: in	std_ulogic;
		hard_rstn				: in	std_ulogic;
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
end entity;

architecture synth of security_engine is

-------------------------------------------------------------------------------
------------------------------------ DEBUG ------------------------------------
-------------------------------------------------------------------------------
	signal	increment_addr	: std_logic_vector(C_ADDR_DBUG-1 downto 0);
	signal	debug_data		: std_logic_vector(63 downto 0);
	signal	log				: std_logic_vector(15 downto 0);
	signal	increment_addr2	: std_logic_vector(C_S_ADDR_DBUG-1 downto 0);
	signal	debug_data2		: std_logic_vector(63 downto 0);
	signal	log2			: std_logic_vector(15 downto 0);
	signal	increment_addr3	: std_logic_vector(C_ADDR_DBUG-1 downto 0);
	signal	debug_data3		: std_logic_vector(63 downto 0);
	signal	log3			: std_logic_vector(15 downto 0);
	signal	increment_addr4	: std_logic_vector(C_ADDR_DBUG-1 downto 0);
	signal	debug_data4		: std_logic_vector(63 downto 0);
	signal	log4			: std_logic_vector(15 downto 0);
	signal	probe			: std_logic_vector(83 downto 0);
-------------------------------------------------------------------------------
------------------------------------ DEBUG ------------------------------------
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Constant declaration 
-------------------------------------------------------------------------------
	constant	C_BUFFER_SIZE		: integer := 2*TC_LINES+1;

-------------------------------------------------------------------------------
-- Type decalration
-------------------------------------------------------------------------------
	type state_type is (S_VALIDATE, S_CALC_PTAG, S_WRITE_PTAG, S_IDLE, S_WAIT_TREE, S_WRITE_ON_TREE);

	-- Tree types
	type T_ENROLL_STATES is (S_INIT, S_LEVEL, S_WAIT_MEM, S_WAIT_MORE, S_READ_PTAGS, S_COMPUTE_PTAG, S_LOOP, S_FINISH);
	type T_TREE_STATES is (S_INIT, S_WAIT, S_HANDLE, S_WAIT_CONTROL, S_SETUP_WRITE, S_WRITE, S_NEW_PTAG, S_DEAL, S_WAIT_DEAL, S_MEM_DEAL, S_SETUP_VERIFY, S_VERIFY, S_FINISH, S_WRITE_BACK, S_ERROR);
	type T_FUZZY_STATES is (S_WAIT_FUZZY, S_FIRST_KEY, S_SECOND_KEY, S_WAIT_KEY, S_FINISH);
	type T_PTAG_BUFFER is array (0 to C_BUFFER_SIZE) of std_logic_vector(C_PTAG_WIDTH-1 downto 0);
	type T_ADDR_BUFFER is array (0 to C_BUFFER_SIZE) of std_logic_vector(TC_ADDR-1 downto 0);
	type T_TYPE_BUFFER is array (0 to C_BUFFER_SIZE) of boolean; 
	
-------------------------------------------------------------------------------
-- Signals decalration 
-------------------------------------------------------------------------------
	signal state_reg       : state_type;
	signal state_next      : state_type;
	signal ptag_mreq       : ptag_mreq_type;
	signal ptag_sval       : ptag_sec_val_type;
	signal ptag_sreq       : ptag_sec_req_type;
	signal cur_ptag        : std_logic_vector(C_PTAG_WIDTH-1 downto 0);
	signal calculated_ptag : std_logic_vector(C_PTAG_WIDTH-1 downto 0);
	signal valid_next      : std_logic;
	signal secure_next     : std_logic;
	signal register_input  : std_logic;
	signal puff_addr       : natural;
	signal ptag_valid      : std_logic;   -- temporary assignment this signaln
	
	signal ready, ptag_ready : std_logic;
	signal computed_ptag     : std_logic_vector (C_PTAG_WIDTH-1 downto 0);

	signal	ptag_gen_valid,
			fuzzy_extractor_finished			: std_logic;

	signal	key1, key2, key						: std_logic_vector (C_KEY_LENGTH-1 downto 0);
	signal	overall_rstn, fuzzy_rstn			: std_logic;
	signal	reboot, enroll, fuzzy_done			: std_logic;

-------------------------------------------------------------------------------
-- Tree Signals
-------------------------------------------------------------------------------
	signal	root_addr, root_level				: natural;
	signal	ptag_root,
			original_ptag_root					: std_logic_vector (C_PTAG_WIDTH-1 downto 0);

	signal	total_data							: integer;

	signal	wrt,
			full_write, ready_to_send,
			write_ptag_a, write_ptag_b,
			write_dirty, ready_control,
			no_ancestor, returning_ptag,
			compute_ptag_a, compute_ptag_b,
			enable_computation,
			hash_the_key, hashing_key,
			requisition, ready_to_receive		: std_logic;
	signal	bus_address, requested_addr,
			returning_addr						: std_logic_vector (TC_ADDR-1 downto 0);

	signal	ptag_to_mem_a, ptag_to_mem_b,
			ptag_to_cache,
			generated_ptag, returned_ptag		: T_PTAG_WORD;
--	signal	ptag_addr_b							: T_PTAG_ADDR_INT;
	signal	ptag_addr_a, ptag_addr_b			: T_PTAG_ADDRESS;

	signal	address								: T_ADDRESS;
	signal	addr_ptags_a, addr_ptags_b,
			address_for_base_ptags				: T_ADDRESS;

	signal	assembling,
			assembled,
			verifying_tree,
			tree_verified						: std_logic;

	signal	key_for_hash,
			data_block,
			block_of_ptags_a,
			block_of_ptags_b					: std_logic_vector (C_PTAG_WIDTH*TC_CHUNK-1 downto 0);

	signal	en_state							: T_ENROLL_STATES;
	signal	state								: T_TREE_STATES;

	signal 	data_in, data_out					: std_logic_vector (C_RAM_WIDTH-1 downto 0);

	constant C_SHIFT_CHUNK	: std_logic_vector (TC_LOG2_CHUNK-1 downto 0) := (others => '0');

	signal	ptag_req_we							: std_logic;
	signal	led_ab, led_ba, led_bb				: std_logic_vector (7 downto 0);
	signal	stored_ptag							: T_PTAG_WORD;

-------------------------------------------------------------------------------
-- Timestamps Signals
-------------------------------------------------------------------------------
	signal	addr_ts								: T_TS_ADDR;
	signal	new_ts								: T_TS_WORD;
	signal	old_ts								: T_TS_WORD;

	signal	update_ts, valid_addr				: std_logic;
begin

-------------------------------------------------------------------------------
-- Verify constants 
-------------------------------------------------------------------------------
	assert 	C_PTAG_ADDR_WIDTH = TC_ADDR
	    report "Address constant problem." severity failure;

-------------------------------------------------------------------------------
-- Combinational assignments
-------------------------------------------------------------------------------
--===============================
--Caio Modifications
--================================
-- Modified by Caio (2017/09/25)

	FE_Enroll_Reboot:
	process (clk, hard_rstn, rstn)
		variable	fstates		: T_FUZZY_STATES;
		variable	tmp_key		: std_logic_vector (C_KEY_LENGTH-1 downto 0);
	begin
		if (hard_rstn = '0') then
			enroll					<= '1';
			reboot					<= '0';
			overall_rstn			<= '0';
			fuzzy_rstn				<= '0';
			ptag_sval.reboot_ready	<= '0';
			key_for_hash			<= (others => '0');
			hashing_key				<= '1';
			fstates					:= S_WAIT_FUZZY;
			tmp_key					:= (others => '0');
		elsif (rstn = '0') then
			enroll					<= '0';
			reboot					<= '1';
			overall_rstn			<= '0';
			fuzzy_rstn				<= '0';
			ptag_sval.reboot_ready	<= '0';
			key_for_hash			<= (others => '0');
			hashing_key				<= '1';
			fstates					:= S_WAIT_FUZZY;
			tmp_key					:= (others => '0');
		elsif (clk = '1' and clk'event) then
			case fstates is
			when S_WAIT_FUZZY =>
				fuzzy_rstn		<= '1';
				if fuzzy_done = '1' then
					fstates									:= S_FIRST_KEY;
					hash_the_key							<= '1';
					key_for_hash(C_KEY_LENGTH-1 downto 0)	<= key2;
					key										<= key1;
				end if;
			when S_FIRST_KEY =>
				if ptag_ready = '1' then
					hash_the_key						<= '0';
					tmp_key(C_PTAG_WIDTH-1 downto 0)	:= calculated_ptag;
					fstates								:= S_WAIT_KEY;
				end if;
			when S_WAIT_KEY =>
				fstates									:= S_SECOND_KEY;
				hash_the_key							<= '1';
				key_for_hash(C_KEY_LENGTH-1 downto 0)	<= key1;
				key										<= key2;
			when S_SECOND_KEY =>
				if ptag_ready = '1' then
					fstates			:= S_FINISH;
					hash_the_key	<= '0';
					tmp_key (2*C_PTAG_WIDTH-1 downto C_PTAG_WIDTH)	:= calculated_ptag;
				end if;
			when S_FINISH =>
				key						<= tmp_key;
				key_for_hash			<= (others => '0');
				enroll					<= '0';
				hashing_key				<= '0';
				reboot					<= '0';
				overall_rstn			<= '1';
				ptag_sval.reboot_ready	<= '1';

			end case;
		end if;
	end process;

	ptag_mreq.address	<=	ptag_addr_a --std_logic_vector(to_unsigned(ptag_addr_a, ptag_mreq.address'length))
							when assembling = '1'													else
							ptag_addr_b --std_logic_vector(to_unsigned(ptag_addr_b, ptag_mreq.address'length))
							when verifying_tree = '1'												else
							ptag_sreq.base_addr(C_PTAG_ADDR+C_BASE_ADDR_BITS_W-1 downto C_BASE_ADDR_BITS_W);

	ptag_mreq.data		<=	ptag_to_mem_a			when assembling = '1'							else
							ptag_to_mem_b			when verifying_tree = '1'						else
							stored_ptag;

    ptag_mreq.we		<=	write_ptag_a			when assembling = '1'							else
							write_ptag_b			when verifying_tree = '1'						else
							ptag_req_we; 

	ptag_to_cache		<=	ptag_mresp_in.data;

	puff_addr			<=	to_integer(unsigned(ptag_sreq.base_addr(31 downto 4)));

	ptag_sval.ptag		<=	stored_ptag;
	 
	address				<=	x"00000000"				when hashing_key = '1'							else
							addr_ptags_a			when assembling = '1'							else
							addr_ptags_b			when verifying_tree = '1'						else
							address_for_base_ptags;

	data_block			<=	key_for_hash			when hashing_key = '1'							else
							block_of_ptags_a		when assembling = '1' 							else
							block_of_ptags_b		when verifying_tree = '1'						else
							ptag_sreq.cache_line;	

	enable_computation	<=	hash_the_key 			when hashing_key = '1'							else
							compute_ptag_a			when assembling = '1'							else
							compute_ptag_b			when verifying_tree = '1'						else
							ptag_gen_valid;

--	ptag_sval.reboot_ready	<= assembled;
	assemble_done			<= assembled;
--	status					<= assembling & verifying_tree & tree_verified & assembled & write_ptag_a & write_ptag_b & compute_ptag_a & compute_ptag_b;
--	status					<= assembling & verifying_tree & tree_verified & assembled & led_ba(7 downto 4);
--	status					<= write_ptag_a & write_ptag_b & compute_ptag_a & compute_ptag_b & led_ba(7 downto 4);
	status					<= led_bb(7 downto 4) & led_ba(7 downto 4);
--	status					<= ptag_ready & tree_verified & assembled & enable_computation & led_ba(7 downto 4);
--	status					<= ptag_ready & tree_verified & assembled & enable_computation & led_bb(7 downto 4);

	Control_inst:
	control generic map (TC_SETS) port map (
		clk					=> clk,
		rstn				=> overall_rstn,
		dirtiness			=> write_dirty,
		incoming_ready		=> ready_to_send,
		ready				=> ready_control,
--		log_in				=> log,
		no_ancestor			=> no_ancestor,
		requesting			=> requisition,
		sending				=> returning_ptag,
		ready_to_receive	=> ready_to_receive,
		wrt					=> wrt,
		root_level			=> root_level,
		bus_address			=> bus_address,
		requested_address	=> requested_addr,
		returning_address	=> returning_addr,
		data_in				=> data_in,
		data_out			=> data_out
	);

	PTAGComputing:    ptag_generator port map (
		clk					=> clk,
		rstn				=> rstn,
		address				=> address,
		key					=> key,
		memory_block		=> data_block,
		enable_computation	=> enable_computation, --ptag_sreq.valid,--ready,
--		load_key_done		=> open, --load_key_done,
		ptag_ready			=> ptag_ready,
		input_data			=> open, -- input_data,
		ptag				=> calculated_ptag --computed_ptag
	);

	fuzzy_instance : fuzzy_extractor 
	port map(
		clk					=> clk,
		rstn				=> fuzzy_rstn,
		reboot				=> reboot,
		enroll				=> enroll,
		key1				=> key1,
		key2				=> key2,
		done				=> fuzzy_done
	);

	timestamp_inst : timestamp PORT MAP (
		address		=> addr_ts,
		clock		=> clk,
		data		=> new_ts,
		wren		=> update_ts,
		q			=> old_ts
	);


-------------------------------------------------------------------------------
-- Auxiliary Registers
-------------------------------------------------------------------------------
	ptag_sval.valid			<= valid_next;
	ptag_sval.line_secure	<= secure_next;

	ptag_sval_out			<= ptag_sval;
	ptag_mreq_out			<= ptag_mreq;

-------------------------------------------------------------------------------
-- Timestamp logic 
-------------------------------------------------------------------------------
	valid_addr				<=	'1' when	(to_integer(unsigned(ptag_sreq.base_addr)) >= C_START_DATA)
									and		(to_integer(unsigned(ptag_sreq.base_addr)) <= C_FINAL_ADDR)
									and		assembled = '0'
									and		enroll_done = '1'												else
								'0';
	addr_ts					<=  std_logic_vector(to_unsigned((to_integer(unsigned(ptag_sreq.base_addr))-C_START_DATA)/(2**(C_BASE_ADDR_BITS_W)), C_TS_ADDR)); 

	address_for_base_ptags	<=	ptag_sreq.base_addr
									when valid_addr = '0'							else
								ptag_sreq.base_addr xor (x"0000" & old_ts)
									when ptag_sreq.wr_ptag = '0'					else
								ptag_sreq.base_addr xor (x"0000" & std_logic_vector(unsigned(old_ts)+1));	

-------------------------------------------------------------------------------
-- State machine combinational circuit
-- handler controller
-------------------------------------------------------------------------------

	state_machine_comb : process( overall_rstn, clk )
		variable address_req					: integer;
		variable use_ptag_tree					: boolean;
		variable ptag_from_cache				: T_PTAG_WORD;


		variable set_set_set					: boolean;
	begin
		if overall_rstn = '0' then
			state_reg				<= S_IDLE;
			valid_next				<= '0';
			secure_next				<= '0';
			ptag_req_we				<= '0';
			ptag_gen_valid			<= '0';
			stored_ptag				<= (others => '0');
			ptag_from_cache			:= (others => '0');
			use_ptag_tree			:= false;
			address_req				:= 0;
		elsif (rising_edge(clk)) then
			case state_reg is
-------------------------------------------------------------------------------
-- IDLE state - no request from the processor
-------------------------------------------------------------------------------
			when S_IDLE =>
				valid_next		<= '0';
				secure_next		<= '0';
				ptag_req_we		<= '0';
				led_bb			<= x"10";
        		ptag_sreq		<= ptag_sreq_in;
        		ptag_sreq		<= ptag_sreq_in;

				use_ptag_tree	:= false;
				address_req		:= to_integer(unsigned(ptag_sreq_in.base_addr));	
				ptag_from_cache	:= (others => '0');

				ptag_gen_valid	<= '0';
				stored_ptag		<= (others => '0');

				update_ts		<= '0';

				if ptag_sreq_in.valid = '1' then
					if ptag_sreq_in.wr_ptag = '1' then
						debug_data4		<= log & x"0BB0" & ptag_sreq_in.base_addr; 
					
					else
						debug_data4		<= log & x"0AA0" & ptag_sreq_in.base_addr; 

					end if;
					increment_addr4	<= std_logic_vector(to_unsigned(to_integer(unsigned(increment_addr4))+1,C_ADDR_DBUG));
					state_reg	<= S_CALC_PTAG;
					if assembled = '1' then
--						if (address_req >= C_START_DATA) and (address_req <= (C_START_DATA + total_data)) then
						if (address_req >= C_START_DATA) and (address_req <= C_FINAL_ADDR) then
							use_ptag_tree	:= true;
						end if;
					end if;
				end if;
-------------------------------------------------------------------------------
-- CALC_PTAG - given the line , calculates the physical tag
-------------------------------------------------------------------------------
			when S_CALC_PTAG =>
				led_bb <= x"30";
				ptag_gen_valid <= '1';
				if ptag_ready = '1' then
					ptag_gen_valid	<= '0';
					stored_ptag		<= calculated_ptag;
					if ptag_sreq.wr_ptag = '1' then
						if use_ptag_tree then	
							state_reg <= S_WRITE_ON_TREE;
						else
							state_reg <= S_WRITE_PTAG;
						end if;
					else
						if use_ptag_tree then
							state_reg <= S_WAIT_TREE;
						else
--						debug_data4		<= log4 & x"0DD0" & ptag_sreq.base_addr; 
--						increment_addr4	<= std_logic_vector(to_unsigned(to_integer(unsigned(increment_addr4))+1,C_ADDR_DBUG));
--					log4			<= std_logic_vector(to_unsigned(to_integer(unsigned(log4))+1, 16));
							state_reg <= S_VALIDATE;
							set_set_set := false;
						end if;
					end if;
				end if;
-------------------------------------------------------------------------------
-- WAIT_TREE - waits the ptag cache to respond and verify the tag presence
-------------------------------------------------------------------------------
			when S_WAIT_TREE =>
				led_bb <= x"C0";
				if tree_verified = '1' then
					state_reg		<= S_VALIDATE;
					ptag_from_cache := returned_ptag;
					set_set_set := false;
					debug_data2		<= stored_ptag(63 downto 48) & x"00" & log2 & x"00" & returned_ptag(63 downto 48); 
					increment_addr2	<= std_logic_vector(to_unsigned(to_integer(unsigned(increment_addr2))+1,C_S_ADDR_DBUG));
					log2			<= std_logic_vector(to_unsigned(to_integer(unsigned(log2))+1, 16));
				end if;	
-------------------------------------------------------------------------------
-- VALIDATE -  determines if the current line is valid ,
-- based on the current ptag from the ptag mem 
-------------------------------------------------------------------------------
			when S_VALIDATE =>
				led_bb <= x"70";
				if not set_set_set then			
					debug_data4		<= stored_ptag(63 downto 48) & x"10" & log2 & x"01" & ptag_mresp_in.data(63 downto 48); 
					increment_addr4	<= std_logic_vector(to_unsigned(to_integer(unsigned(increment_addr2))+1,C_ADDR_DBUG));
					log2			<= std_logic_vector(to_unsigned(to_integer(unsigned(log2))+1, 16));
					set_set_set		:= true;
				end if;
				if ptag_mresp_in.data = stored_ptag or (ptag_from_cache = stored_ptag and use_ptag_tree) then
					valid_next	<= '1';
					secure_next	<= '1';
					state_reg	<= S_IDLE;
--COMMENTED--				else 
--COMMENTED--					valid_next	<= '0';
--COMMENTED--					secure_next	<= '0';
--COMMENTED--					state_reg	<= S_IDLE;
				end if;
-------------------------------------------------------------------------------
-- WRITE PTAG - if its the case, calculate the new ptag
-- and writes in to memory
-------------------------------------------------------------------------------
			when S_WRITE_PTAG =>
				led_bb <= x"F0";
				ptag_req_we		<= '1';
				state_reg		<= S_IDLE;
				valid_next		<= '1';
				secure_next		<= '1';
				update_ts		<= valid_addr;
				new_ts			<= std_logic_vector(unsigned(old_ts)+1);
			when S_WRITE_ON_TREE =>
				if tree_verified = '1' then
					--ptag_req_we		<= '1';
					state_reg		<= S_IDLE;
					valid_next		<= '1';
					secure_next		<= '1';
				end if;
			when others =>
				state_reg			<= S_IDLE;
			end case;
		end if;
	end process;

-------------------------------------------------------------------------------
-------------------------------- TREE ASSEMBLE -------------------------------
-------------------------------------------------------------------------------
	TreeAssemble:
	process (clk, overall_rstn)
		variable	aux, shift		: natural;

		variable	addr			: bit_vector (C_ADDR_WIDTH-1 downto 0);
		variable	ptag			: bit_vector (C_PTAG_WIDTH-1 downto 0); 
		variable	new_ptag,
					tmp_ptag		: std_logic_vector (C_PTAG_WIDTH-1 downto 0); 

		variable	cnt_all_ptags,
					max_level		: integer;

		variable	cnt,
					cnt_tree,
					cnt_line		: integer;

		variable	aux_addr		: integer;
		variable	bit_aux_addr	: std_logic_vector (C_ADDR_WIDTH-1 downto 0);	
		variable	std_aux_addr	: std_logic_vector (TC_ADDR-1 downto 0);	

		variable	write_after		: boolean;
		variable	use_new_ptag	: boolean;

		variable	ptag_buffer		: T_PTAG_BUFFER; 
		variable	addr_buffer		: T_ADDR_BUFFER;
		variable	type_buffer		: T_TYPE_BUFFER;
		variable	pos_buffer		: natural;	

		variable	j, i			: natural;
	begin
		if (overall_rstn = '0') then
			aux					:= 0;
			cnt					:= 0;
			i					:= 0;
			root_level			<= 0;
			root_addr			<= 0;
			compute_ptag_a		<= '0';
			write_ptag_a		<= '0';
			assembling			<= '0';
			assembled			<= '0';
			original_ptag_root	<= (others => '0');
			en_state			<= S_INIT;
		elsif (rising_edge(clk)) then
			case en_state is
			when S_INIT =>
				if enroll_tree = '1' then
					j			:= 0;

					assembling	<= '1';
					assembled	<= '0';
					en_state	<= S_LEVEL;
--					en_state	<= S_FINISH;

					-- Compute the Merkle Tree height
					cnt_all_ptags	:= 1;
					max_level		:= 1;

					cnt_tree	:= (1+C_FINAL_ADDR-C_START_DATA)/(C_WORD_WIDTH);
					total_data	<= cnt_tree * (2**C_BASE_ADDR_BITS_W);

					while cnt_all_ptags < cnt_tree loop
						max_level		:= max_level + 1;
						cnt_all_ptags	:= (cnt_all_ptags)*(TC_CHUNK);
					end loop;
				end if;
				led_ab <= x"10";
			when S_LEVEL =>
				led_ab <= x"20";
				-- All ptags of ptags are stored in addresses with the MSB equal to 1;

				i			:= 0;		
				cnt_line	:= 0;

				block_of_ptags_a	<= (others => '0');
				en_state			<= S_READ_PTAGS;

			when S_READ_PTAGS => 
				led_ab <= x"30";
				write_ptag_a	<= '0';
--				aux				:= C_START_DATA+j*(2**(C_PTAG_ADDR+C_BASE_ADDR_BITS_W--1))+i*(2**C_BASE_ADDR_BITS_W);
				aux				:= C_START_DATA+i*(2**(j*TC_LOG2_CHUNK+C_BASE_ADDR_BITS_W));
				bit_aux_addr	:= std_logic_vector(to_unsigned(aux, C_ADDR_WIDTH));
				std_aux_addr	:= (others => '0');
				if j = 0 then
					std_aux_addr	:= bit_aux_addr(C_PTAG_ADDR+C_BASE_ADDR_BITS_W-1 downto C_BASE_ADDR_BITS_W);
				else
--					std_aux_addr	:= '1' & std_logic_vector(to_unsigned(j,TC_LOG2_DEPTH)) & std_logic_vector(to_unsigned(0, j*TC_LOG2_CHUNK)) &  bit_aux_addr(C_PTAG_ADDR+C_BASE_ADDR_BITS_W-TC_LOG2_DEPTH-2 downto C_BASE_ADDR_BITS_W+j*TC_LOG2_CHUNK);
--COMMENTED--					std_aux_addr	:= (others => '0');
--COMMENTED--					std_aux_addr(C_PTAG_ADDR-1 downto C_PTAG_ADDR-TC_LOG2_DEPTH-1) := '1' & std_logic_vector(to_unsigned(j,TC_LOG2_DEPTH));
--COMMENTED--					std_aux_addr(C_PTAG_ADDR-TC_LOG2_DEPTH-(j*TC_LOG2_CHUNK) downto 0) := bit_aux_addr(C_PTAG_ADDR+C_BASE_ADDR_BITS_W-TC_LOG2_DEPTH downto C_BASE_ADDR_BITS_W+(j*TC_LOG2_CHUNK));
					std_aux_addr	:= assemble_address(bit_aux_addr(C_PTAG_ADDR+C_BASE_ADDR_BITS_W-1 downto C_BASE_ADDR_BITS_W), j);
				end if;
				shift			:= to_integer(unsigned(std_aux_addr(TC_LOG2_CHUNK-1 downto 0)));
--------------------- DEBUG ---------------------
				debug_data3			<= bit_aux_addr & x"000" & std_aux_addr;
				increment_addr3		<= std_logic_vector(to_unsigned(to_integer(unsigned(increment_addr3))+1,C_ADDR_DBUG));
--------------------- DEBUG ---------------------
				
				ptag_addr_a		<= std_aux_addr;
				en_STATE		<= S_WAIT_MEM;
			when S_WAIT_MEM =>
--------------------- DEBUG ---------------------
				debug_data3			<= log3 & x"0" & std_logic_vector(to_unsigned(i, 12)) & x"0" & std_logic_vector(to_unsigned(cnt_tree,12)) & x"0000";
				increment_addr3		<= std_logic_vector(to_unsigned(to_integer(unsigned(increment_addr3))+1,C_ADDR_DBUG));
--------------------- DEBUG ---------------------
				en_STATE		<= S_WAIT_MORE;
			when S_WAIT_MORE =>
				led_ab <= x"40";
				block_of_ptags_a ((shift+1)*C_PTAG_WIDTH-1 downto shift*C_PTAG_WIDTH) <= ptag_to_cache;
--				cnt_line		:= cnt_line + 1;
				cnt_line		:= shift+1; 
				compute_ptag_a	<= '0';

				-- Verifies if the number of PTAGs read is equal to the chunk size
				-- Or there is no more PTAG for a specfic level
				if (cnt_line >= TC_CHUNK) or (i = cnt_tree-1) then
--					std_aux_addr	:= '1' & std_logic_vector(to_unsigned(j+1,TC_LOG2_DEPTH)) & std_logic_vector(to_unsigned(0, (j+1)*TC_LOG2_CHUNK)) &  bit_aux_addr(C_PTAG_ADDR+C_BASE_ADDR_BITS_W-TC_LOG2_DEPTH-2 downto C_BASE_ADDR_BITS_W+(j+1)*TC_LOG2_CHUNK);
--COMMENTED--					std_aux_addr	:= (others => '0');
--COMMENTED--					std_aux_addr(C_PTAG_ADDR-1 downto C_PTAG_ADDR-TC_LOG2_DEPTH-1) := '1' & std_logic_vector(to_unsigned((j+1),TC_LOG2_DEPTH)); 
--COMMENTED--					std_aux_addr(C_PTAG_ADDR-TC_LOG2_DEPTH-((j+1)*TC_LOG2_CHUNK) downto 0) := bit_aux_addr(C_PTAG_ADDR+C_BASE_ADDR_BITS_W-TC_LOG2_DEPTH downto C_BASE_ADDR_BITS_W+((j+1)*TC_LOG2_CHUNK));
					std_aux_addr	:= assemble_address(bit_aux_addr(C_PTAG_ADDR+C_BASE_ADDR_BITS_W-1 downto C_BASE_ADDR_BITS_W), j+1);
					addr_ptags_a	<= std_logic_vector(to_unsigned(0, C_ADDR_WIDTH-TC_ADDR)) & std_aux_addr;
					compute_ptag_a	<= '1';

					en_state		<= S_COMPUTE_PTAG;
				else
					en_state		<= S_READ_PTAGS;
				end if;
				i				:= i + 1;

--------------------- DEBUG ---------------------
				debug_data3			<= log3 & x"0" & bit_aux_addr(C_PTAG_ADDR-1 downto 0) & x"0" & std_aux_addr;
				increment_addr3		<= std_logic_vector(to_unsigned(to_integer(unsigned(increment_addr3))+1,C_ADDR_DBUG));
--------------------- DEBUG ---------------------
			when S_COMPUTE_PTAG =>
				led_ab <= x"50";
				write_ptag_a		<= '0';
				if ptag_ready = '1' then
					compute_ptag_a		<= '0';
					cnt_line			:= 0;	
					ptag_to_mem_a		<= calculated_ptag;
--					ptag_to_mem_a		<= x"000000000000" & '0' & '0' & std_aux_addr;
--					ptag_addr_a			<= To_integer(unsigned(std_aux_addr));
					ptag_addr_a			<= std_aux_addr; 
					write_ptag_a		<= '1';
					block_of_ptags_a	<= (others => '0');
					
					if (i >= cnt_tree) then
						en_state	<= S_LOOP;
					else 
						en_state	<= S_READ_PTAGS;
					end if;
--------------------- DEBUG ---------------------
					debug_data3			<= log3 & x"0" & std_logic_vector(to_unsigned(i, 12)) & x"0" & std_logic_vector(to_unsigned(cnt_tree,12)) & x"0000";
--COMMENTED--					debug_data3			<= calculated_ptag;
					increment_addr3		<= std_logic_vector(to_unsigned(to_integer(unsigned(increment_addr3))+1,C_ADDR_DBUG));
					log3				<= std_logic_vector(to_unsigned(to_integer(unsigned(log3))+1, 16));
--------------------- DEBUG ---------------------
				end if;
			when S_LOOP =>
				led_ab <= x"60";
				write_ptag_a	<= '0';

				j := j + 1;
				if j < max_level-1 then
					aux			:= cnt_tree mod TC_CHUNK;
					cnt_tree	:= cnt_tree / TC_CHUNK;
					if aux /= 0 then
						cnt_tree := cnt_tree + 1;
					end if;

--------------------- DEBUG ---------------------
					debug_data3			<= log3 & x"0" & std_logic_vector(to_unsigned(j, 4)) & x"0" & std_logic_vector(to_unsigned(cnt_tree,12)) & x"0" & std_logic_vector(to_unsigned(max_level, 4)) &  std_logic_vector(to_unsigned(cnt_all_ptags, 16));
					increment_addr3		<= std_logic_vector(to_unsigned(to_integer(unsigned(increment_addr3))+1,C_ADDR_DBUG));
					log3				<= std_logic_vector(to_unsigned(to_integer(unsigned(log3))+1, 16));
--------------------- DEBUG ---------------------
				
					en_state	<= S_LEVEL;	
				else
					en_state	<= S_FINISH;	
				end if;
			when S_FINISH =>
				led_ab <= x"70";
				root_level			<= j;
				root_addr			<= To_integer(unsigned(std_aux_addr));
				original_ptag_root	<= ptag_to_mem_a;

				assembled			<= '1';
				assembling			<= '0';
			end case;
		end if;
	end process;

-------------------------------------------------------------------------------
------------------------------------- TREE ------------------------------------
-------------------------------- DATA INTEGRITY -------------------------------
--------------------------------- VERIFICATION --------------------------------
-------------------------------------------------------------------------------
	DataIntegrityVerification:
	process (clk, overall_rstn)
		variable	aux, shift		: natural;

		variable	addr			: bit_vector (C_ADDR_WIDTH-1 downto 0);
		variable	ptag			: bit_vector (C_PTAG_WIDTH-1 downto 0); 
		variable	new_ptag,
					tmp_ptag		: std_logic_vector (C_PTAG_WIDTH-1 downto 0); 

		variable	cnt,
					cnt_tree,
					cnt_line		: integer;

		variable	aux_addr		: integer;
		variable	bit_aux_addr	: std_logic_vector (C_ADDR_WIDTH-1 downto 0);	
		variable	std_aux_addr	: std_logic_vector (TC_ADDR-1 downto 0);	

		variable	write_after		: boolean;
		variable	use_new_ptag	: boolean;

		variable	ptag_buffer		: T_PTAG_BUFFER; 
		variable	addr_buffer		: T_ADDR_BUFFER;
		variable	type_buffer		: T_TYPE_BUFFER;
		variable	pos_buffer		: natural;	

		variable	i_buffer		: natural;	
		variable	root_unsaved,
					need_to_set,
					found			: boolean;
	begin
		if (overall_rstn = '0') then
			state			<= S_INIT;
			bus_address		<= (others => '0');
			wrt				<= '0';
			ready_to_send	<= '0';
			write_ptag_b	<= '0';
			compute_ptag_b	<= '0';
			tree_verified	<= '0';
			write_dirty		<= '0';
			ptag_root		<= (others => '0');
			verifying_tree	<= '0';
			returned_ptag	<= (others => '0');

			root_unsaved	:= true;
			need_to_set		:= true;
			new_ptag		:= (others => '0');
			tmp_ptag		:= (others => '0');
		elsif (rising_edge(clk)) then
			case state is
			when S_INIT =>
				tree_verified	<= '0';
				write_ptag_b	<= '0';
				if state_reg = S_WAIT_TREE or state_reg = S_WRITE_ON_TREE then
					verifying_tree	<= '1';
					returned_ptag	<= (others => '0');
					state			<= S_HANDLE;
--					state			<= S_FINISH;
					aux_addr		:= To_Integer(unsigned(ptag_sreq.base_addr));	
					bit_aux_addr	:= ptag_sreq.base_addr;
					if ptag_root = (ptag_root'range => '0') then
						ptag_root	<= original_ptag_root;
					end if;
					led_ba		<= x"10";
--					debug_data		<= original_ptag_root;
--					increment_addr	<= std_logic_vector(to_unsigned(to_integer(unsigned(increment_addr))+1,C_ADDR_DBUG));
--					log				<= std_logic_vector(to_unsigned(to_integer(unsigned(log))+1, 16));

				end if;
				led_ba		<= x"50";
			when S_HANDLE =>
				led_ba		<= x"20";
				aux				:= 0;
				pos_buffer		:= 0;
				new_ptag		:= stored_ptag;
				use_new_ptag	:= false;
				need_to_set		:= true;
--				if (aux_addr >= C_START_DATA) and (aux_addr <= (C_START_DATA + total_data)) then
					ptag_buffer(pos_buffer)	:= new_ptag;
					addr_buffer(pos_buffer)	:= bit_aux_addr(TC_ADDR+C_BASE_ADDR_BITS_W-1 downto C_BASE_ADDR_BITS_W);
					if state_reg = S_WRITE_ON_TREE then
						type_buffer(pos_buffer) := true;
				debug_data		<= log & x"00EEE00" & bit_aux_addr(TC_ADDR+C_BASE_ADDR_BITS_W-1 downto C_BASE_ADDR_BITS_W);
					else
						type_buffer(pos_buffer) := false;
				debug_data		<= log & x"00FFF00" & bit_aux_addr(TC_ADDR+C_BASE_ADDR_BITS_W-1 downto C_BASE_ADDR_BITS_W);
					end if;


					pos_buffer				:= pos_buffer + 1;
					state					<= S_WAIT;
				increment_addr	<= std_logic_vector(to_unsigned(to_integer(unsigned(increment_addr))+1,C_ADDR_DBUG));
				log				<= std_logic_vector(to_unsigned(to_integer(unsigned(log))+1, 16));
--				else
--					state <= S_FINISH;
--				end if;
			when S_WAIT =>
				led_ba			<= x"30";
				ready_to_send	<= '0';
				ready_control	<= '1';
				wrt				<= '0';
				aux				:= 0;

				if requisition = '1' then
				-- The PTAG was not in the Cache
				-- Returns father address and PTAG when they are present in the Cache
					ready_control	<= '0';
					state			<= S_DEAL;
					aux				:= to_integer(unsigned(returning_addr(TC_LOG2_CHUNK-1 downto 0)));
					tmp_ptag		:= data_out((aux+1)*C_PTAG_WIDTH-1 downto C_PTAG_WIDTH*aux);
					debug_data		<= x"ABC" & '0' & no_ancestor & '0' & '0' & requested_addr & x"00" & returning_addr;
					increment_addr	<= std_logic_vector(to_unsigned(to_integer(unsigned(increment_addr))+1,C_ADDR_DBUG));
					aux				:= 0;

				-- PTAG Cache verification didn't have the chunk and all were dirty
				-- Write back the chunk to the memory and update the father
				elsif returning_ptag = '1' then

					ptag_buffer(pos_buffer)	:= new_ptag;
					addr_buffer(pos_buffer)	:= requested_addr;
					type_buffer(pos_buffer) := use_new_ptag;
					pos_buffer				:= pos_buffer + 1;
					use_new_ptag			:= false;

					ready_control	<= '0';
					debug_data		<= x"CBA0" & requested_addr & x"00" & returning_addr;
					increment_addr	<= std_logic_vector(to_unsigned(to_integer(unsigned(increment_addr))+1,C_ADDR_DBUG));
					-- Setup the block to write in the PTAG memory

					-- Setup to compute the new ptag of the ancestor
					std_aux_addr	:= find_next_address(returning_addr);
--COMMENTED--					if returning_addr(TC_ADDR-1) = '1' then 
--COMMENTED--						aux				:= to_integer(unsigned(returning_addr(TC_ADDR-2 downto TC_ADDR-TC_LOG2_DEPTH-1)))+1;
--COMMENTED--						std_aux_addr	:= '1' & std_logic_vector(to_unsigned(aux, TC_LOG2_DEPTH)) & C_SHIFT_CHUNK & returning_addr(TC_ADDR-TC_LOG2_DEPTH-2 downto TC_LOG2_CHUNK); 
--COMMENTED--					else
--COMMENTED--						aux				:= 1;
--COMMENTED--						std_aux_addr	:= '1' & std_logic_vector(to_unsigned(aux, TC_LOG2_DEPTH)) & returning_addr(TC_ADDR-TC_LOG2_DEPTH downto TC_LOG2_CHUNK); 
--COMMENTED--					end if;

					block_of_ptags_b	<= data_out;
					addr_ptags_b		<= std_logic_vector(to_unsigned(0, C_ADDR_WIDTH-TC_ADDR)) & std_aux_addr;
					compute_ptag_b		<= '1';
					write_ptag_b		<= '0';
					aux					:= 0;

					state			<= S_WRITE_BACK;
				elsif ready_to_receive = '1' then
					ready_control		<= '0';
					debug_data			<= x"EE0" & bus_address & x"000" & std_aux_addr;
					increment_addr		<= std_logic_vector(to_unsigned(to_integer(unsigned(increment_addr))+1,C_ADDR_DBUG));
					if use_new_ptag then
						debug_data			<= x"DD0" & bus_address & x"000" & std_aux_addr;
						increment_addr		<= std_logic_vector(to_unsigned(to_integer(unsigned(increment_addr))+1,C_ADDR_DBUG));
						bus_address			<= std_aux_addr;
						block_of_ptags_b	<= data_out;
						state				<= S_SETUP_WRITE;
					else
						if pos_buffer > 0 then 
							ready_to_send	<= '1';
							pos_buffer		:= pos_buffer - 1;
							found			:= false;
							new_ptag		:= ptag_buffer(pos_buffer);
							std_aux_addr	:= addr_buffer(pos_buffer);
							use_new_ptag	:= type_buffer(pos_buffer);
							bus_address		<= addr_buffer(pos_buffer);
						 
							state			<= S_WAIT_CONTROL;
							debug_data		<= x"AA0" & std_logic_vector(to_unsigned(pos_buffer, 4)) & new_ptag(63 downto 40) & x"0" & addr_buffer(pos_buffer);
							increment_addr	<= std_logic_vector(to_unsigned(to_integer(unsigned(increment_addr))+1,C_ADDR_DBUG));
						else
							debug_data		<= x"BB0" & bus_address & x"000" & std_aux_addr;
							increment_addr	<= std_logic_vector(to_unsigned(to_integer(unsigned(increment_addr))+1,C_ADDR_DBUG));
							if bus_address = bit_aux_addr(TC_ADDR+C_BASE_ADDR_BITS_W-1 downto C_BASE_ADDR_BITS_W) and need_to_set then
								aux				:= to_integer(unsigned(bit_aux_addr(TC_LOG2_CHUNK+C_BASE_ADDR_BITS_W-1 downto C_BASE_ADDR_BITS_W)));
								need_to_set		:= false;
								debug_data		<= x"CC0" & bus_address & x"000" & std_aux_addr;
								returned_ptag	<= data_out((aux+1)*C_PTAG_WIDTH-1 downto C_PTAG_WIDTH*aux);
							end if;
							state			<= S_FINISH;
						end if;
					end if;
				end if;
--			when S_SETUP_WRITE_BACK =>
--				-- PTAGGen MEM address
--				block_of_ptags_b	<= data_out;
--				addr_ptags_b		<= std_logic_vector(to_unsigned(0, C_ADDR_WIDTH-TC_ADDR)) & std_aux_addr;
--				compute_ptag_b		<= '1';
--				write_ptag_b		<= '0';
--				aux					:= 0;
--
--				state			<= S_WRITE_BACK;
			when S_WRITE_BACK =>
				led_ba			<= x"40";
				if aux < TC_CHUNK then
					ptag_addr_b		<= returning_addr(TC_ADDR-1 downto TC_LOG2_CHUNK) & std_logic_vector(to_unsigned(aux, TC_LOG2_CHUNK));
					write_ptag_b	<= '1';
					ptag_to_mem_b	<= block_of_ptags_b((C_PTAG_WIDTH*(aux+1))-1 downto C_PTAG_WIDTH*aux); 
					debug_data		<= x"110" & block_of_ptags_b((C_PTAG_WIDTH*(aux+1))-1 downto C_PTAG_WIDTH*aux+36) & x"0" & returning_addr(TC_ADDR-1 downto TC_LOG2_CHUNK) & std_logic_vector(to_unsigned(aux, TC_LOG2_CHUNK));
					increment_addr	<= std_logic_vector(to_unsigned(to_integer(unsigned(increment_addr))+1,C_ADDR_DBUG));
					aux				:= aux + 1;
				else
					write_ptag_b	<= '0';
					state			<= S_NEW_PTAG;
				end if;
			when S_WAIT_CONTROL =>
				if ready_to_receive = '0' then
					state				<= S_WAIT;
				end if;
			when S_DEAL =>
--				debug_data		<= tmp_ptag;
--				increment_addr	<= std_logic_vector(to_unsigned(to_integer(unsigned(increment_addr))+1,C_ADDR_DBUG));
				led_ba			<= x"80";
				-- Finishes Cache Requisition for ancestors
--				std_aux_addr	:= requested_addr(TC_ADDR-1 downto TC_LOG2_CHUNK) & C_SHIFT_CHUNK; 
				ptag_addr_b		<= requested_addr(TC_ADDR-1 downto TC_LOG2_CHUNK) & std_logic_vector(to_unsigned(aux, TC_LOG2_CHUNK));
				debug_data		<= x"CCAA00FFF00" & requested_addr(TC_ADDR-1 downto TC_LOG2_CHUNK) & std_logic_vector(to_unsigned(aux, TC_LOG2_CHUNK));
				increment_addr	<= std_logic_vector(to_unsigned(to_integer(unsigned(increment_addr))+1,C_ADDR_DBUG));
									--ptag_addr		<= to_integer(unsigned(bit_aux_addr(C_PTAG_ADDR+C_BASE_ADDR_BITS_W-1 downto C_BASE_ADDR_BITS_W))) + i*(2**C_BASE_ADDR_BITS_W);
				state			<= S_WAIT_DEAL;
			when S_WAIT_DEAL =>
				state			<= S_MEM_DEAL;
			when S_MEM_DEAL =>
				led_ba			<= x"90";
				block_of_ptags_b (((aux+1)*C_PTAG_WIDTH-1) downto aux*C_PTAG_WIDTH) <= ptag_to_cache;
				debug_data		<= ptag_to_cache;
				increment_addr	<= std_logic_vector(to_unsigned(to_integer(unsigned(increment_addr))+1,C_ADDR_DBUG));

				if aux < TC_CHUNK-1 then
					aux				:= aux + 1;
					state			<= S_DEAL;
				else
					bus_address		<= requested_addr;
					state			<= S_SETUP_VERIFY;
					
				end if;
			when S_SETUP_VERIFY => 
				led_ba			<= x"A0";
				-- Set to read PTAG's ancestor and compute a new PTAG from the chunk brought from PTAG-MEM
				aux				:= to_integer(unsigned(std_aux_addr(TC_ADDR-2 downto TC_ADDR-TC_LOG2_DEPTH-1)))+1;
				std_aux_addr	:= returning_addr;
			--	std_aux_addr	:= '0' & std_logic_vector(to_unsigned(aux, )) & C_SHIFT_CHUNK & std_aux_addr(TC_ADDR-TC_LOG2_DEPTH-2 downto TC_LOG2_CHUNK); 
				-- PTAG MEM address
				-- Ignore when verify PTAG ROOT
--				ptag_addr_b		<= to_integer(unsigned(std_aux_addr));
				found			:= false;
				for i in 0 to C_BUFFER_SIZE loop
					if i < pos_buffer and addr_buffer(i) = std_aux_addr and type_buffer(i) then
						found			:= true;
						i_buffer		:= i;
						debug_data		<= x"FF0" & std_logic_vector(to_unsigned(0, C_PTAG_WIDTH-TC_ADDR-12)) & std_aux_addr;
					end if;
				end loop; 
				if not found then 
					ptag_addr_b		<= std_aux_addr;
					debug_data		<= x"EE0" & std_logic_vector(to_unsigned(0, C_PTAG_WIDTH-TC_ADDR-12)) & std_aux_addr;
				end if;

				-- PTAGGen MEM address
				addr_ptags_b	<= std_logic_vector(to_unsigned(0, C_ADDR_WIDTH-TC_ADDR)) & std_aux_addr;
				compute_ptag_b	<= '1';
				increment_addr	<= std_logic_vector(to_unsigned(to_integer(unsigned(increment_addr))+1,C_ADDR_DBUG));

				-- Compares Ancestors's PTAG and Computed PTAG
				state <= S_VERIFY;
			when S_VERIFY =>
				led_ba			<= x"C0";
--				debug_data(13 downto 0)		<= ptag_addr_b;
--				debug_data					<= ptag_to_cache;
--				debug_data					<= '0' & '0' & std_logic_vector(to_unsigned(root_addr,TC_ADDR)) & x"0000" & addr_ptags_b;
--				increment_addr				<= std_logic_vector(to_unsigned(to_integer(unsigned(increment_addr))+1,C_ADDR_DBUG));
--				debug_data		<= ptag_to_cache;
--				increment_addr	<= std_logic_vector(to_unsigned(to_integer(unsigned(increment_addr))+1,C_ADDR_DBUG));
				if ptag_ready = '1' then
					-- This verification avoid PTAG collision with the PTAG_ROOT		
--					debug_data		<= calculated_ptag;
--					increment_addr	<= std_logic_vector(to_unsigned(to_integer(unsigned(increment_addr))+1,C_ADDR_DBUG));
					if to_integer(unsigned(ptag_addr_b)) = root_addr then
						led_ba			<= x"B0";
--						debug_data		<= calculated_ptag; 
						if calculated_ptag = ptag_root then
							state		<= S_SETUP_WRITE;
						else
--							write(l, String'("((((((((((((((((((((((((( PTAG NMI )))))))))))))))))))))))))"));
							debug_data		<= x"222200" & calculated_ptag(63 downto 48) & x"00" & ptag_root(63 downto 48);
							increment_addr	<= std_logic_vector(to_unsigned(to_integer(unsigned(increment_addr))+1,C_ADDR_DBUG));
							state <= S_ERROR;
						end if; 
					else
						if no_ancestor = '1' then
							led_ba			<= x"70";
							if ptag_to_cache = calculated_ptag or (ptag_buffer(i_buffer) = calculated_ptag and found) then
								state <= S_SETUP_WRITE;
							else
--								write(l, String'("((((((((((((((((((((((((( PTAG NMI )))))))))))))))))))))))))"));
								debug_data		<= x"222200" & calculated_ptag(63 downto 48) & x"00" & ptag_to_cache(63 downto 48);
								increment_addr	<= std_logic_vector(to_unsigned(to_integer(unsigned(increment_addr))+1,C_ADDR_DBUG));
								state <= S_ERROR;
							end if; 
						else
							led_ba			<= x"00";
							if tmp_ptag = calculated_ptag or (ptag_buffer(i_buffer) = calculated_ptag and found) then
								state <= S_SETUP_WRITE;
							else
--								write(l, String'("((((((((((((((((((((((((( PTAG NMI )))))))))))))))))))))))))"));
								debug_data		<= x"222200" & calculated_ptag(63 downto 48) & x"00" & tmp_ptag(63 downto 48);
								increment_addr	<= std_logic_vector(to_unsigned(to_integer(unsigned(increment_addr))+1,C_ADDR_DBUG));
								state <= S_ERROR;
							end if; 
						end if;
					end if;
					compute_ptag_b	<= '0';
				end if;
			when S_SETUP_WRITE => 
				led_ba			<= x"D0";
				aux 			:= to_integer(unsigned(bus_address(TC_LOG2_CHUNK-1 downto 0)));
				if use_new_ptag then
					block_of_ptags_b (((aux+1)*C_PTAG_WIDTH)-1 downto aux*C_PTAG_WIDTH) <= new_ptag;
					debug_data		<= x"CCCC0" & new_ptag(63 downto 56)  & x"08" & bus_address & x"0" & std_logic_vector(to_unsigned(aux, 4));
				else
					debug_data		<= x"CCCC0" & new_ptag(63 downto 56)  & x"00" & bus_address & x"0" & std_logic_vector(to_unsigned(aux, 4));
				end if;
				increment_addr	<= std_logic_vector(to_unsigned(to_integer(unsigned(increment_addr))+1,C_ADDR_DBUG));
				state			<= S_WRITE;
			when S_WRITE =>
				led_ba			<= x"E0";
				if use_new_ptag then
					use_new_ptag	:= false;
					write_dirty		<= '1';
				else
					write_dirty		<= '0';
				end if;
				if bus_address = bit_aux_addr(TC_ADDR+C_BASE_ADDR_BITS_W-1 downto C_BASE_ADDR_BITS_W) and need_to_set then
					need_to_set		:= false;
					returned_ptag	<= block_of_ptags_b ((aux+1)*C_PTAG_WIDTH-1 downto aux*C_PTAG_WIDTH);
					debug_data		<= x"BBBB" & '0' & write_dirty & '0' & '0' & bus_address & x"0" & block_of_ptags_b ((aux+1)*C_PTAG_WIDTH-1 downto aux*C_PTAG_WIDTH+44);
				else
					debug_data		<= x"BBBA" & '0' & write_dirty & '0' & '0' & bus_address & x"0" & block_of_ptags_b ((aux+1)*C_PTAG_WIDTH-1 downto aux*C_PTAG_WIDTH+44);
				end if;
				increment_addr	<= std_logic_vector(to_unsigned(to_integer(unsigned(increment_addr))+1,C_ADDR_DBUG));
				data_in			<= block_of_ptags_b;	
				ready_to_send	<= '1';
				wrt				<= '1';
				state			<= S_WAIT_CONTROL;
			when S_NEW_PTAG =>
				led_ba			<= x"F0";
				if ptag_ready = '1' then
					new_ptag		:= calculated_ptag;
					compute_ptag_b	<= '0';

					-- Now we buffer the new PTAG father.
					state			<= S_WAIT;

					-- Update the root
					aux				:= to_integer(unsigned(std_aux_addr));
					if aux = root_addr then
						ptag_root	<= new_ptag;
					debug_data		<= x"DDBB" & std_logic_vector(to_unsigned(pos_buffer, 12)) & x"0" & new_ptag(63 downto 32);
					increment_addr	<= std_logic_vector(to_unsigned(to_integer(unsigned(increment_addr))+1,C_ADDR_DBUG));
					else
					debug_data		<= x"DDAA" & std_logic_vector(to_unsigned(pos_buffer, 12)) & x"0" & new_ptag(63 downto 32);
					increment_addr	<= std_logic_vector(to_unsigned(to_integer(unsigned(increment_addr))+1,C_ADDR_DBUG));
						found			:= false;
						for i in 0 to C_BUFFER_SIZE loop
							if i < pos_buffer and addr_buffer(i) = std_aux_addr and type_buffer(i) then
								found			:= true;
								i_buffer		:= i;
							end if;
						end loop; 
						if found then
							ptag_buffer(i_buffer)	:= new_ptag;
						else
							ptag_buffer(pos_buffer)	:= new_ptag;
							addr_buffer(pos_buffer)	:= std_aux_addr;
							type_buffer(pos_buffer)	:= true;
							pos_buffer				:= pos_buffer + 1;
						end if;
					end if;
				end if;
			when S_FINISH =>
--				write(l, String'(" ************************************ FINISH ************************************ "));
				verifying_tree	<= '0';
				tree_verified	<= '1';
				led_ba			<= x"60";
--				debug_data		<= ((data_out((aux+1)*C_PTAG_WIDTH-1 downto C_PTAG_WIDTH*aux) xor stored_ptag) and x"FFFFFFFFFFFF0000") or x"0000000000004444";
--				debug_data		<= ((returned_ptag xor stored_ptag) and x"FFFFFFFFFFFF0000") or x"0000000000004444";
				debug_data		<= ((stored_ptag) and x"FFFFFFFF00000000") or x"0000000000004444";
				if state_reg /= S_WAIT_TREE and state_reg /= S_WRITE_ON_TREE then
--					debug_data		<= ((returned_ptag xor stored_ptag) and x"FFFFFFFFFFFF0000") or x"0000000000005555";
					debug_data		<= ((returned_ptag) and x"FFFFFFFF00000000") or x"0000000000005555";
				increment_addr	<= std_logic_vector(to_unsigned(to_integer(unsigned(increment_addr))+1,C_ADDR_DBUG));
					state		<= S_INIT;
				end if;
			when S_ERROR => 
			end case; 
		end if;
	end process;

	log_out			<= log;

--COMMENTED--	debug_memory_inst1 : debug_memory PORT MAP (
--COMMENTED--			address	 => increment_addr,
--COMMENTED--			clock	 => clk,
--COMMENTED--			data	 => debug_data,
--COMMENTED--			wren	 => '1',
--COMMENTED--			q		 => open
--COMMENTED--		);
--COMMENTED--
--COMMENTED--	u0 : component ProbeTree
--COMMENTED--		port map (
--COMMENTED--			source     => probe,     --    sources.source
--COMMENTED--			probe      => addr_ptags_a(C_PTAG_ADDR-1 downto 0) & computed_ptag,      --     probes.probe
--COMMENTED--			source_clk => clk -- source_clk.clk
--COMMENTED--		);
--COMMENTED--
--COMMENTED--	debug_memory_inst2 : s_debug_memory PORT MAP (
--COMMENTED--			address	 => increment_addr2,
--COMMENTED--			clock	 => clk,
--COMMENTED--			data	 => debug_data2,
--COMMENTED--			wren	 => '1',
--COMMENTED--			q		 => open
--COMMENTED--		);
--COMMENTED--	debug_memory_inst3 : debug_memory PORT MAP (
--COMMENTED--			address	 => increment_addr3,
--COMMENTED--			clock	 => clk,
--COMMENTED--			data	 => debug_data3,
--COMMENTED--			wren	 => '1',
--COMMENTED--			q		 => open
--COMMENTED--		);
--COMMENTED--
--COMMENTED--	debug_memory_inst4 : debug_memory PORT MAP (
--COMMENTED--			address	 => increment_addr4,
--COMMENTED--			clock	 => clk,
--COMMENTED--			data	 => debug_data4,
--COMMENTED--			wren	 => '1',
--COMMENTED--			q		 => open
--COMMENTED--		);
end architecture;
