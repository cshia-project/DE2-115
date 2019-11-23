-------------------------------------------------------------------------------
-- Title      : BUS HANDLER component of CSHIA
-- Project    : CSHIA
-------------------------------------------------------------------------------
-- File       : (original ptag_handler.vhd) (current bus_handler.vhd)
-- Author     : augusto  
-- Company    : 
-- Created    : 2015-08-22
-- Last update: 2016-04-19
-- Platform   : 
-- Standard   : VHDL'93/02
-------------------------------------------------------------------------------
-- Description: this block executes security check in a given memory range
-- for eead and writing
-------------------------------------------------------------------------------
-- Copyright (c) 2015  
-------------------------------------------------------------------------------
-- Revisions  : 
-- Date        Version  Author  Description
-- 2015-08-22  1.0      augusto Created
-- Date        Version  Author  Description
-- 2018-2019   1.2      Caio	- Added Compatibility to new Security Engine
--								- Added Debug Components and Code
--								- Name change for the purpose of Coherence
-------------------------------------------------------------------------------
--TODO LIST
-- put memory range check(for memory mapped IO)
-- separate instrunctions from data in the line
-- put the constants as functions of the generic
-- when the last instruction is a write , identify this  and flush the line in to the main memory
-- When out of range  the  last data read may  go straigth to the processor  since the 
--           handler waits the hready  from the bus   maybe this is an securit issue
-- Add the speculation of 1 , always execute pass the line allowing the engine to hide the latency
-- all the comparisons for the out of range must be done with the base address
-- UPDATES TO NEW GRLIBs

-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use IEEE.numeric_std.all;
library grlib;
use grlib.amba.all;
library CSHIA;
use CSHIA.handler_pkg.all;
use CSHIA.DBUG_PKG.all;

entity bus_handler is
	generic (
		G_LINE_SIZE : natural := 16; -- size of the cahche line to be loaded
		G_LINE_WIDTH : natural := 32); -- size of the word in bits
	port (
		clk : in std_ulogic;
		rstn : in std_ulogic;
		bypass_in : in std_logic;
		enroll_done : out std_logic;
		assemble_done : in std_logic;
		-------------------------------------------------------------------------------
		log_in : in std_logic_vector(15 downto 0);
		-------------------------------------------------------------------------------
		ptag_sreq_out : out ptag_sec_req_type;
		ptag_sval_in : in ptag_sec_val_type;
		--AHB interface
		ahbi_in : in ahb_mst_in_type;
		ahbi_out : out ahb_mst_in_type;
		ahbo_in : in ahb_mst_out_type;
		ahbo_out : out ahb_mst_out_type;
		state_out : out std_logic_vector(7 downto 0);
		watchdog_en : in std_logic
	);
end entity;

architecture synth of bus_handler is

	-------------------------------------------------------------------------------
	-- Type decalration
	-------------------------------------------------------------------------------
	type line_filler_state_type is (READ, READ_GRANT, SERVE_LEON, WRITE_LINE, IDLE, UNSAFE, OUT_OF_RANGE, S_WAIT_PTAG_WRITE, S_WAIT_PTAG_READ);
	type asc_buffer_state_type is (IDLE, REPLACE, SWITCH);

	-------------------------------------------------------------------------------
	-- Signals decalration 
	-------------------------------------------------------------------------------
	signal ahbi_to_leon : ahb_mst_in_type;
	signal ahbo_to_bus : ahb_mst_out_type;
	signal lf_state_reg : line_filler_state_type;
	signal lf_state_next : line_filler_state_type;
	signal base_addr : std_logic_vector(31 downto 0);
	signal bus_addr : std_logic_vector(31 downto 0);
	signal addr_cnt_en : std_logic;
	--line buffer
	signal line_tag : std_logic_vector(31 downto 0);
	signal line_buffer : line_buffer_type;
	--/line buffer
	signal addr_cnt : natural range 0 to C_LINE_SIZE + 1;
	signal addr_cnt_clr : std_logic;
	signal line_buffer_wr : std_logic;
	signal line_buffer_addr : natural;
	signal line_buffer_data : std_logic_vector(G_LINE_WIDTH downto 0);
	signal line_buffer_wr_data : std_logic_vector(G_LINE_WIDTH - 1 downto 0);
	signal save_addr_en : std_logic;
	signal line_full : std_logic;
	signal line_status : std_logic;
	signal line_status_reg : std_logic;
	signal lb_we_reg : std_logic;
	signal lb_we_next : std_logic;
	signal dirty_line : std_logic;
	signal dirty_line_reg : std_logic;
	signal leon_write : std_logic;
	signal is_first_trans_r : std_logic;
	signal is_first_trans : std_logic;
	signal vp_state : natural range 0 to C_LINE_SIZE + 1;
	signal raw_line : std_logic_vector((C_LINE_SIZE * C_WORD_WIDTH) - 1 downto 0);
	signal line_secure_reg : std_logic;
	signal clear_line_sec : std_logic;
	--associative_buffer signals
	signal asc_bufferr : associative_buffer;
	signal asc_buffer_st : associative_buffer_st;
	--signal asc_buffer_cnt        : natural range 0 to C_ASSOCIATIVE_BUFF_SIZE;
	--   signal asc_buffer_next     : natural range 0 to C_ASSOCIATIVE_BUFF_SIZE-1;
	signal asc_keep_cur_reg : std_logic;
	signal asc_keep_cur_next : std_logic;
	signal asc_buffer_cur : natural range 0 to C_ASSOCIATIVE_BUFF_SIZE - 1;
	signal asc_buffer_hit_line : natural range 0 to C_ASSOCIATIVE_BUFF_SIZE - 1;
	signal asc_buffer_full : std_logic;
	signal asc_buffer_hit : std_logic;
	signal asc_buffer_ready : std_logic;
	signal asc_buffer_state_reg : asc_buffer_state_type;
	signal asc_buffer_state_next : asc_buffer_state_type;
	signal internal_bypass : std_logic;
	signal enroll : std_logic;
	signal enroll_status : std_logic;
	signal enroll_next : std_logic;
	signal enroll_addr : natural;
	signal enroll_addr_en : std_logic;
	signal master_address : std_logic_vector(31 downto 0);
	signal master_address_prev : std_logic_vector(31 downto 0);
	signal ptag_sreq : ptag_sec_req_type;
	signal internal_req : std_logic; -- when the bus doesnt conclude the transaction
	signal internal_req_next : std_logic; -- when the bus doesnt conclude the transaction
	--the data signal must be stable in the data  phase when after the ptag handler takes the control of the bus
	signal hwdata_to_bus : std_logic_vector(G_LINE_WIDTH - 1 downto 0);
	signal watchdog : natural;
	signal bus_granted : std_logic; -- if the master changed we need to wait one cycle for the grant
	signal bus_granted_next : std_logic; -- if the master changed we need to wait one cycle for the grant
	-------------------------------------------------------------------------------
	------------------------------------ DEBUG ------------------------------------
	-------------------------------------------------------------------------------
	signal write_address : integer range 0 to 255;
	signal log_cache : std_logic_vector(15 downto 0);

	signal increment_addr : std_logic_vector(C_ADDR_DBUG - 1 downto 0);
	signal debug_data : std_logic_vector(63 downto 0);
	signal log : std_logic_vector(15 downto 0);

	signal increment_addr2 : std_logic_vector(C_ADDR_DBUG - 1 downto 0);
	signal debug_data2 : std_logic_vector(63 downto 0);
	signal log2 : std_logic_vector(15 downto 0);

	signal increment_addr3 : std_logic_vector(C_ADDR_DBUG - 1 downto 0);
	signal debug_data3 : std_logic_vector(63 downto 0);
	signal log3 : std_logic_vector(15 downto 0);

	signal increment_addr4 : std_logic_vector(C_ADDR_DBUG - 1 downto 0);
	signal debug_data4 : std_logic_vector(63 downto 0);
	signal log4 : std_logic_vector(15 downto 0);
	-------------------------------------------------------------------------------
	------------------------------------ DEBUG ------------------------------------
	-------------------------------------------------------------------------------
begin
	-----------------------------------------------------------------------------
	-- Auxialiary registers
	--base_addr        - the tag of the current line used for comparison with te
	--                   requsted data
	--line_status_reg  - if the line is valid
	--lb_we_reg        - write enable for the line buffer
	--dirty_line_reg   - when the processos writes in the line , then it needs to
	--                   be rwrited in the memory  before been replaced
	--is_first_trans_r - the first trans in the bus needs to be NONSEQ this flag
	--                   is used to set the trans type
	-----------------------------------------------------------------------------
	aux_registers_seq : process (clk, rstn) is
	begin -- process addr_comletion_seq
		if rstn = '0' then -- asynchronous reset (active low)
			lb_we_reg <= '0';
			is_first_trans_r <= '1';
			asc_keep_cur_reg <= '0';
			bus_granted <= '0';

			for I in 0 to C_ASSOCIATIVE_BUFF_SIZE - 1 loop
				asc_buffer_st(I).valid <= '0';
				asc_buffer_st(I).dirty <= '0';
				asc_buffer_st(I).address <= (others => '0');
				asc_buffer_st(I).secure <= '1';

			end loop;
		elsif clk'event and clk = '1' then -- rising clock edge
			--if the previous state ofthe line filler was a write  
			--the current line must be maintened
			asc_keep_cur_reg <= asc_keep_cur_next;
			-- the scontrol only sets the base  address  when the processor request
			-- a transaction
			if save_addr_en = '1' and enroll = '0' then
				asc_buffer_st(asc_buffer_cur).address <= master_address(31 downto C_BASE_ADDR_BITS_W) &
				std_logic_vector(to_unsigned(0, C_BASE_ADDR_BITS_W));
			end if;
--COMMENTED--			if clear_line_sec = '1' then
--COMMENTED--				asc_buffer_st(asc_buffer_cur).secure <= '0';
--COMMENTED--			elsif ptag_sval_in.valid = '1' and enroll = '0' then
--COMMENTED--				asc_buffer_st(asc_buffer_cur).secure <= ptag_sval_in.line_secure;
--COMMENTED--			end if;
--COMMENTED--			asc_buffer_st(asc_buffer_cur).valid <= line_status;
			if clear_line_sec = '1' then
				asc_buffer_st(asc_buffer_cur).secure	<= '0';
				asc_buffer_st(asc_buffer_cur).valid		<= '0';
			elsif ptag_sval_in.valid = '1' and enroll = '0' then
				asc_buffer_st(asc_buffer_cur).secure	<= ptag_sval_in.line_secure;
				asc_buffer_st(asc_buffer_cur).valid		<= ptag_sval_in.valid;
			end if;
			lb_we_reg <= lb_we_next;
			asc_buffer_st(asc_buffer_cur).dirty <= dirty_line;
			is_first_trans_r <= is_first_trans;
			ptag_sreq_out.wr_ptag <= ptag_sreq.wr_ptag;
			ptag_sreq_out.valid <= ptag_sreq.valid;
			ptag_sreq_out.base_addr <= ptag_sreq.base_addr;
			bus_granted <= bus_granted_next;

			write_address <= write_address + 1;
			log_cache <= std_logic_vector(to_unsigned(to_integer(unsigned(log_cache)) + 1, 16));

		end if;
	end process aux_registers_seq;
	ptag_sreq_out.cache_line <= ptag_sreq.cache_line;

	--aliasing
	dirty_line_reg <= asc_buffer_st(asc_buffer_cur).dirty;
	line_status_reg <= asc_buffer_st(asc_buffer_cur).valid;
	base_addr <= asc_buffer_st(asc_buffer_cur).address when enroll = '0' else
		std_logic_vector(to_unsigned(enroll_addr, master_address'length));
	line_secure_reg <= asc_buffer_st(asc_buffer_cur).secure; --aliasing 

	process (ahbo_in.haddr, enroll_addr, enroll, internal_req)
	begin
		if enroll = '0' then
			master_address <= ahbo_in.haddr;
			if internal_req = '1' then
				master_address <= master_address_prev;
			end if;
		else
			master_address <= std_logic_vector(to_unsigned(enroll_addr, master_address'length));
		end if;

	end process;
	-------------------------------------------------------------------------------
	--ENROLL addr
	-------------------------------------------------------------------------------
	enroll_done <= enroll_status;

	enroll_addr_seq : process (clk, rstn) is
	begin -- process addr_completion_seq
		if rstn = '0' then -- asynchronous reset (active low)
			enroll_addr <= to_integer(unsigned(C_MIN_ADDR_RANGE));
			enroll <= '1';
			enroll_status <= '0';
			master_address_prev <= (others => '0');
		elsif clk'event and clk = '1' then -- rising clock edge
			if internal_req = '0' then
				master_address_prev <= master_address;
			end if;
			--		if enroll_status = '0' then
			--			if enroll_addr_en = '1'  and enroll = '1' then
			--				if enroll_addr < to_integer(unsigned(C_MAX_ADDR_RANGE)) then
			--					enroll_addr <= enroll_addr+C_LINE_SIZE*4;
			--				else
			--					enroll_status <= '1';
			--				end if;
			--			end if;
			--		else
			--			if assemble_done = '1' then	
			--				enroll <= '0';
			--			end if;
			--		end if;
			if enroll_addr_en = '1' and enroll = '1' then
				if enroll_addr + (C_LINE_SIZE * 4) <= to_integer(unsigned(C_MAX_ADDR_RANGE)) then
					enroll_addr <= enroll_addr + C_LINE_SIZE * 4;
				else
					--				if lf_state_next  = IDLE then
					enroll_status <= '1';
					--					if assemble_done = '1' then	
					enroll <= '0';
					--					end if;
					--				end if;
				end if;
			end if;
		end if;
	end process enroll_addr_seq;

	-------------------------------------------------------------------------------
	-- Associative buffer control
	-------------------------------------------------------------------------------
	asc_buffer_ctrl_seq : process (clk, rstn) is
		variable asc_buffer_next : natural range 0 to C_ASSOCIATIVE_BUFF_SIZE;
		variable asc_buffer_cnt : natural range 0 to C_ASSOCIATIVE_BUFF_SIZE;

	begin -- process addr_completion_seq
		if rstn = '0' then -- asynchronous reset (active low)
			--       asc_buffer_state_reg <= IDLE;
			asc_buffer_next := 0;
			asc_buffer_cur <= 0;
			asc_buffer_full <= '0';
			asc_buffer_cnt := 0;
		elsif clk'event and clk = '1' then -- rising clock edge
			--       asc_buffer_state_reg  <= asc_buffer_state_next;
			asc_buffer_cnt := asc_buffer_cnt + 1;
			if asc_buffer_next = C_ASSOCIATIVE_BUFF_SIZE - 1 then
				asc_buffer_cnt := 0;
			end if;
			if asc_buffer_full = '0' then
				if line_full = '1' and lf_state_reg = READ then
					if asc_buffer_next = C_ASSOCIATIVE_BUFF_SIZE - 1 then
						asc_buffer_full <= '1';
					else
						asc_buffer_next := asc_buffer_next + 1;
					end if;
					if enroll = '1' then
						asc_buffer_next := 0;
					end if;
				end if;
			elsif lf_state_reg /= IDLE then
				--running counter for replacement
				asc_buffer_next := asc_buffer_cnt;--to_integer(unsigned(asc_bufferr(asc_buffer_cur)(0)(9 downto 7)));
			end if;
			asc_buffer_ready <= '0';
			if (ahbo_in.hbusreq = '1' or internal_req = '1') and lf_state_reg = IDLE and line_secure_reg = '1' then
				asc_buffer_ready <= '1';
				if asc_buffer_hit = '1' then
					asc_buffer_cur <= asc_buffer_hit_line;
				elsif asc_keep_cur_reg = '0' then
					asc_buffer_cur <= asc_buffer_next;
				end if;
			end if;

		end if;
	end process asc_buffer_ctrl_seq;

	-----------------------------------------------------------------------------
	-- Hit verification
	-----------------------------------------------------------------------------
	process (asc_buffer_st, master_address)
	begin
		asc_buffer_hit <= '0';
		asc_buffer_hit_line <= 0;

		for I in 0 to C_ASSOCIATIVE_BUFF_SIZE - 1 loop
			if asc_buffer_st(I).address = master_address(31 downto C_BASE_ADDR_BITS_W) &
				std_logic_vector(to_unsigned(0, C_BASE_ADDR_BITS_W)) and asc_buffer_st(I).valid = '1' then
				asc_buffer_hit_line <= I;
				asc_buffer_hit <= '1';
			end if;
		end loop;
	end process;
	-------------------------------------------------------------------------------
	-- Watchdog
	-------------------------------------------------------------------------------
	watchdog_seq : process (clk, rstn) is
		variable state_v : std_logic;
	begin -- process addr_completion_seq
		if rstn = '0' then -- asynchronous reset (active low)
			watchdog <= 0;
			state_v := '0';
		elsif clk'event and clk = '1' then -- rising clock edge
			if lf_state_reg = SERVE_LEON then
				state_v := '1';
			end if;
			if lf_state_reg = IDLE and state_v = '1' then
				watchdog <= watchdog + 1;
			else
				watchdog <= 0;
			end if;
		end if;
	end process;

	-----------------------------------------------------------------------------
	--Tranform the line into  raw data;
	-----------------------------------------------------------------------------
	process (asc_bufferr)
	begin
		for I in 1 to C_LINE_SIZE loop
			--report "lala => " & integer'image(C_LINE_SIZE-I);
			raw_line((I * C_WORD_WIDTH) - 1 downto ((I - 1) * C_WORD_WIDTH)) <= asc_bufferr(asc_buffer_cur)(C_LINE_SIZE - I)(C_WORD_WIDTH downto 1);
		end loop;
	end process;
	-------------------------------------------------------------------------------
	-- State machine sequential
	-------------------------------------------------------------------------------
	state_machine_seq : process (clk, rstn) is
	begin -- process addr_completion_seq
		if rstn = '0' then -- asynchronous reset (active low)
			lf_state_reg <= IDLE;
			internal_req <= '0';
		elsif clk'event and clk = '1' then -- rising clock edge
			lf_state_reg <= lf_state_next;
			internal_req <= internal_req_next;
		end if;
	end process state_machine_seq;

	-----------------------------------------------------------------------------
	--State machine combinational circuit
	--  handler controller
	-----------------------------------------------------------------------------
	state_machine_comb : process (addr_cnt, ahbi_in.hgrant(0), ahbi_in.hirq, enroll,
		ahbi_in.hready, ahbi_in.scanen, ahbi_in.testen,
		ahbi_in.testoen, ahbi_in.testrst,
		master_address(31 downto C_BASE_ADDR_BITS_W),
		ahbo_in.hbusreq, ahbo_in.hconfig,
		ahbo_in.hindex, ahbo_in.hirq, ahbo_in.htrans,
		ahbo_in.hwrite, base_addr, dirty_line_reg, bus_granted,
		is_first_trans_r, lf_state_reg, line_full, internal_req,
		line_secure_reg, line_status_reg, asc_keep_cur_reg,
		raw_line, asc_buffer_ready, ptag_sval_in.valid, ptag_sval_in.reboot_ready)
		variable local_addres_v : unsigned(31 downto 0);
	begin

		--default values
		ahbi_to_leon.hgrant <= (others => '0'); -- bus grant
		ahbi_to_leon.hready <= '0'; --  bus  ready;
		ahbi_to_leon.hresp <= HRESP_OKAY; -- response type
		ahbi_to_leon.hirq <= ahbi_in.hirq; -- interrupt result bus
		ahbi_to_leon.testen <= ahbi_in.testen; -- scan test enable
		ahbi_to_leon.testrst <= ahbi_in.testrst; -- scan test reset
		ahbi_to_leon.scanen <= ahbi_in.scanen; -- scan enable
		ahbi_to_leon.testoen <= ahbi_in.testoen; -- test output enable 
		ahbo_to_bus.hbusreq <= '0'; -- bus request
		ahbo_to_bus.hlock <= '0'; -- lock request
		ahbo_to_bus.htrans <= HTRANS_IDLE; -- transfer type
		ahbo_to_bus.hwrite <= '0'; -- read/write
		ahbo_to_bus.hsize <= HSIZE_WORD; -- transfer size
		ahbo_to_bus.hburst <= HBURST_INCR; -- burst type
		ahbo_to_bus.hprot <= (others => '0'); -- protection control
		ahbo_to_bus.hirq <= ahbo_in.hirq; -- interrupt bus
		ahbo_to_bus.hconfig <= ahbo_in.hconfig; -- memory access reg.
		ahbo_to_bus.hindex <= ahbo_in.hindex; -- integer range 0 to NAHBMST-1;  
		lb_we_next <= '0'; -- line buffer write enable
		dirty_line <= dirty_line_reg;
		leon_write <= '0'; -- if the data beeing write in the
		-- line buffer comes from the processor
		addr_cnt_clr <= '0'; -- clear the line buffer addr counter
		lf_state_next <= lf_state_reg; -- current state
		save_addr_en <= '0'; --  save current base address
		addr_cnt_en <= '0'; -- enables the addres counter
		line_status <= line_status_reg; -- validity of the line
		is_first_trans <= is_first_trans_r; -- the trans ins the bus is 
		--ptag security signals
		ptag_sreq.cache_line <= raw_line;
		ptag_sreq.base_addr <= base_addr;
		ptag_sreq.valid <= '0';
		ptag_sreq.wr_ptag <= '0';
		clear_line_sec <= '0';
		asc_keep_cur_next <= asc_keep_cur_reg;
		enroll_addr_en <= '0';
		--combinational buss asignment to bus
		local_addres_v := shift_left((to_unsigned(addr_cnt, local_addres_v'length)), 2);
		ahbo_to_bus.haddr <= std_logic_vector(unsigned(base_addr) + local_addres_v);
		vp_state <= 0;
		internal_bypass <= '0';
		internal_req_next <= internal_req;
		state_out <= std_logic_vector(to_unsigned(255, 8));
		bus_granted_next <= bus_granted;
		case lf_state_reg is
				-------------------------------------------------------------------------------
				-- IDLE state - no request from the processor
				-------------------------------------------------------------------------------
			when IDLE =>
				if watchdog > 250 and watchdog_en = '1' then
					ahbo_to_bus.htrans <= HTRANS_BUSY;
				end if;
				state_out <= std_logic_vector(to_unsigned(1, 8));

				--report "im at idle";
				vp_state <= 1;
				addr_cnt_clr <= '1'; --clear the address counter
				if enroll = '1' then
					if ptag_sval_in.reboot_ready = '1' then
						lf_state_next <= READ_GRANT; -- I nee do read a new line
						report "im enrolling " & integer'image(enroll_addr);
					end if;
				else
					if (ahbo_in.hbusreq = '1' or internal_req = '1') and asc_buffer_ready = '1' then --Verify if a request came
						--verify if I already have the address and if the line is valid
						--if  	ahbo_in.htrans = HTRANS_IDLE then 										 
						--	internal_req_next <= '0';  
						--end if;
						if base_addr = master_address(31 downto C_BASE_ADDR_BITS_W) &
							std_logic_vector(to_unsigned(0, C_BASE_ADDR_BITS_W)) and line_status_reg = '1' then
							lf_state_next <= SERVE_LEON; -- I have the line and can provide
							-- the request to the processor
							internal_req_next <= '0';
						else
							if unsigned(master_address) < unsigned(C_MAX_ADDR_RANGE) and unsigned(master_address) > unsigned(C_MIN_ADDR_RANGE) then
								if line_secure_reg = '1' then

									ahbo_to_bus.hbusreq <= '1'; --request  bus
									if ahbi_in.hgrant(0) = '1'then
										lf_state_next <= READ_GRANT; -- I nee do read a new line
										bus_granted_next <= '1';
									end if;
									if (not dirty_line_reg) = '1' then -- if the line is dirty do not
										-- save the tag 
										save_addr_en <= '1'; --save base address
									end if;
								else
									report "im at not secure";
								end if;
							else
								--if ahbi_in.hready = '0' then
								internal_bypass <= '1';
								lf_state_next <= OUT_OF_RANGE;
								--end if;
							end if;
						end if;
					end if;
				end if;
				-------------------------------------------------------------------------------
				-- OUT OF RANGE - the addres is not protected by the security engine
				-------------------------------------------------------------------------------		  
			when OUT_OF_RANGE =>
				state_out <= std_logic_vector(to_unsigned(32, 8));
				internal_bypass <= '1';
				-- if ahbo_in.hbusreq = '1'  then  --Verify if a request came
				if unsigned(master_address) > unsigned(C_MIN_ADDR_RANGE) and unsigned(master_address) < unsigned(C_MAX_ADDR_RANGE) then
					if ahbi_in.hready = '1' then
						lf_state_next <= IDLE;
--						ahbi_to_leon.hresp <= HRESP_ERROR; -- response type
					end if;
					internal_bypass <= '0';
					--ahbi_to_leon.hready  <=  not ahbi_in.hready;        --  sometime the bus still not finished with the operation 
					if ahbo_in.htrans /= HTRANS_IDLE and ahbo_in.hbusreq = '0' then
						internal_req_next <= '1'; -- the bus can star transactions when granted even not asserting hreq
					end if;
					--  end if;
				end if;
				-------------------------------------------------------------------------------
				-- READ GRANT- a request came, request the grant to read or write
				-------------------------------------------------------------------------------        
			when READ_GRANT => --Wait for    ahbi_to_leon.hgrant <= '1';
				--report "im atread grant";
				vp_state <= 2;
				state_out <= std_logic_vector(to_unsigned(2, 8));
				is_first_trans <= '1'; -- set that this is the first transaction
				ahbo_to_bus.hbusreq <= '1'; --request  bus
-------------------------------------------
--- WORKS FOR LONG RUNTIME BENCHMARKS -----
--				lf_state_next <= IDLE;
--- BETTER REMOVE TO SMALL ONES	-----------			
-------------------------------------------				
				if ahbi_in.hgrant(0) = '0' then
					bus_granted_next <= '0';
-------------------------------------------
--- OR ALSO TRY THIS
--				lf_state_next <= IDLE;
-------------------------------------------				
				elsif ahbi_in.hready = '1' then
					bus_granted_next <= '1';
				end if;
				--if ahbi_in.hgrant(0) = '1' and bus_granted ='1'  then
				if ahbi_in.hgrant(0) = '1' then

					ahbo_to_bus.htrans <= HTRANS_NONSEQ; -- set the first transaction as
					-- non sequential

					addr_cnt_clr <= '0';
					if ahbi_in.hready = '1' then
						if enroll = '1' then
							lf_state_next <= READ;
						elsif dirty_line_reg = '1' then
							--              lf_state_next      <= WRITE_LINE;  -- if the line is dirty go to
							-- write state to write all
							-- line in the main memory
							lf_state_next <= S_WAIT_PTAG_WRITE; -- if the line is dirty go to
							-- write state to write all
							--              ahbo_to_bus.hwrite <= '1';
							ptag_sreq.valid <= '1'; -- now  that the line is full  the ptag can be calculated
							ptag_sreq.wr_ptag <= '1'; -- the line needs to be changed
							-- send the security write bit

						else
							lf_state_next <= READ; -- go to read state to read a new line
							--  --report "################################################################################ READ STATE";

							--lb_we_next   <= '1';          -- enable the write to the line buffer
							--addr_cnt_en  <= ahbi_in.hready ; --TODO verify this line and the line above why it doenst  work in hardware
						end if;
					end if;
				end if;
-----------------------------------------------------------------
-- Try to make sequential PTAG computation and write back
-----------------------------------------------------------------
			when S_WAIT_PTAG_WRITE =>
				state_out <= std_logic_vector(to_unsigned(254, 8));
				if ptag_sval_in.valid = '1' and ptag_sval_in.line_secure = '1' then
--					asc_keep_cur_next		<= '1';
					ahbo_to_bus.hwrite		<= '1';
--					ahbo_to_bus.hbusreq		<= '1';
					lf_state_next			<= WRITE_LINE;
				end if;
				-----------------------------------------------------------------------
				-- READ -  read a  new line  to the line buffer
				-----------------------------------------------------------------------
			when READ =>
				asc_keep_cur_next <= '0';
				vp_state <= 3;
				state_out <= std_logic_vector(to_unsigned(4, 8));
				ahbo_to_bus.hbusreq <= '1'; --request  bus
				-- check if the grant is set  by the arbiter
				if ahbi_in.hgrant(0) = '1'then
					is_first_trans <= '0';
					if is_first_trans_r = '0' then -- set the trasns type
						ahbo_to_bus.htrans <= HTRANS_SEQ;
					else
						ahbo_to_bus.htrans <= HTRANS_NONSEQ;
					end if;
					addr_cnt_clr <= '0';
					lb_we_next <= '1'; -- enable the write to the line buffer
					--the counter  of the addres need to be increased only if the ready
					--of the previous transaction is asserted
					addr_cnt_en <= ahbi_in.hready or is_first_trans_r; --and lb_we_reg; TODO CHECK THIS MODIFICATION

					-- if the line is full  the request from the procesor can be attended
					if line_full = '1' then
						report "im at read";
						lf_state_next <= IDLE;

						if enroll = '1' then
							lf_state_next <= UNSAFE;
							ptag_sreq.valid <= '1';
							ptag_sreq.wr_ptag <= '1';
						else

							line_status <= '1';
							clear_line_sec <= '1';
						end if;
						lb_we_next <= '0'; -- enable the write to the line buffer
						ptag_sreq.valid <= '1'; -- now  that the line is full  the ptag can be calculated
					end if;
				else
					lf_state_next <= READ_GRANT; -- if the grant is removed need to be reaquired
				end if;
				-------------------------------------------------------------------------------
				-- SERVE_LEON - when the line contains the request  serve the processor request
				-------------------------------------------------------------------------------
			when SERVE_LEON => -- serve leon request
				--report "im at serve leon";
				vp_state <= 4;
				state_out <= std_logic_vector(to_unsigned(16, 8));
				ahbi_to_leon.hgrant(0) <= '1'; -- the the grant to the processor
				ahbi_to_leon.hready <= '1'; -- indicates that it can begin the requests
				--verify if I already have the address
				if base_addr = master_address(31 downto C_BASE_ADDR_BITS_W) &
					std_logic_vector(to_unsigned(0, C_BASE_ADDR_BITS_W)) and line_status_reg = '1' then
					lf_state_next <= SERVE_LEON;
					-- if the processor perform a write operation the line is then dirty
					-- and the control signalizes that is the processor writing
					if ahbo_in.hwrite = '1' then
						dirty_line <= '1';
						leon_write <= '1';
					end if;
				else
					if (unsigned(master_address) > unsigned(C_MAX_ADDR_RANGE) or unsigned(master_address) < unsigned(C_MIN_ADDR_RANGE))
						and ahbo_in.htrans /= HTRANS_IDLE then
						internal_bypass <= '1';
						lf_state_next <= OUT_OF_RANGE;
					else
						if ahbo_in.htrans /= HTRANS_IDLE and ahbo_in.hbusreq = '0' then
							internal_req_next <= '1'; -- the bus can star transactions when granted even not asserting hreq
						end if;
						lf_state_next <= IDLE; -- the addrs requested doesn't match
						-- the base addres so go back to
						-- idle to check the address
						ahbi_to_leon.hgrant(0) <= '0'; -- remove the grant
					end if;
				end if;
				if ahbo_in.htrans = HTRANS_IDLE then
					internal_req_next <= '0';
				end if;
				if ahbo_in.hbusreq = '0' or ahbo_in.htrans = HTRANS_IDLE then -- remove the grant is the processor
					-- finishes the requests	
					ahbi_to_leon.hgrant(0) <= '0';
				end if;
				-------------------------------------------------------------------------------
				-- WRITE LINE - if the line is dirty the write back in the mais memory
				-------------------------------------------------------------------------------
			when WRITE_LINE =>
				--report "im at write line";
				asc_keep_cur_next <= '1';
--COMMENTED--				ptag_sreq.wr_ptag <= '1'; -- the line needs to be changed
				-- send the security write bit
				state_out <= std_logic_vector(to_unsigned(8, 8));
				vp_state <= 5;

				ahbo_to_bus.hbusreq <= '1'; --request  bus
				ahbo_to_bus.hwrite <= '1'; --set write operation 

				if ahbi_in.hgrant(0) = '1'then
					is_first_trans <= '0';
					if is_first_trans_r = '0' then
						ahbo_to_bus.htrans <= HTRANS_SEQ;
					else
						ahbo_to_bus.htrans <= HTRANS_NONSEQ;
					end if;
					addr_cnt_clr <= '0';
					addr_cnt_en <= ahbi_in.hready;

					if line_full = '1' then
						lf_state_next <= IDLE;
						dirty_line <= '0';
					end if;
				else
					lf_state_next <= READ_GRANT;
				end if;

				-------------------------------------------------------------------------------
				-- UNSAFE - The execution is not safe anymore
				-------------------------------------------------------------------------------
			when UNSAFE =>
				state_out <= std_logic_vector(to_unsigned(64, 8));
				--report "im at unsafe";
				lf_state_next <= UNSAFE;
				if ptag_sval_in.valid = '1' then
					lf_state_next <= IDLE;
					enroll_addr_en <= '1';

				end if;
			when others =>
				vp_state <= 0;
				state_out <= std_logic_vector(to_unsigned(128, 8));
				lf_state_next <= IDLE;
		end case;
	end process;

	-----------------------------------------------------------------------------
	-- Counter for the addres
	-----------------------------------------------------------------------------
	-- purpose: this block counts the address  to fill the inernal line buffer.
	-- type   : sequential
	-- inputs : clk, rstn
	-- outputs: 
	addr_cnt_seq : process (clk, rstn) is
	begin -- process addr_cnt_seq
		if rstn = '0' then -- asynchronous reset (active low)
			addr_cnt <= 0;
			line_full <= '0';
			line_buffer_addr <= 0;
		elsif clk'event and clk = '1' then -- rising clock edge
			if addr_cnt_clr = '1' then
				addr_cnt <= 0;
				line_full <= '0';
				line_buffer_addr <= 0;

			elsif addr_cnt_en = '1' then --elsif  line_buffer_wr = '1' then 
				--        if addr_cnt /= C_LINE_SIZE-1 then
				if addr_cnt < C_LINE_SIZE - 1 then
					addr_cnt <= addr_cnt + 1;
				else
					line_full <= '1';
				end if;
				line_buffer_addr <= addr_cnt;

			end if;
			-- This certifies we only update the addr when the line was written
			--       if line_buffer_wr = '1' then
			--      line_buffer_addr <= addr_cnt;
			--       end if;
		end if;
	end process addr_cnt_seq;
	-- line_buffer_addr <= addr_cnt; --TODO check why this need to be  combinational in hardware

	-----------------------------------------------------------------------------
	-- Bypass mode - bypasses all signals for debug
	-----------------------------------------------------------------------------
	process (ahbi_to_leon, ahbi_in, bypass_in)
	begin
		if (bypass_in = '0' and internal_bypass = '0') then
			ahbi_out <= ahbi_to_leon;
		else
			ahbi_out <= ahbi_in;
		end if;
		if lf_state_reg = SERVE_LEON and internal_bypass = '1' then
			ahbi_out.hrdata <= ahbi_to_leon.hrdata;
		end if;
	end process;
	ahbo_out <= ahbo_to_bus when (bypass_in = '0' and internal_bypass = '0') else ahbo_in;
	-----------------------------------------------------------------------------
	-- LINE BUFFER
	-----------------------------------------------------------------------------
	-- purpose: this block emulates a one line cache
	-- type   : sequential
	-- inputs : clk, rstn
	-- outputs:
	line_buffer_data <= ahbi_in.hrdata & ahbi_in.hcache; --'0'; -- ahbi_in.hcache;  -- concatenates the
	-- data with the cache
	-- information
	line_buffer_wr <= ahbi_in.hready and lb_we_reg; -- only writes when enabled
	-- and when the dat is ready

	--process (clk) is begin
	--	if (rising_edge(clk)) then
	--		if ahbi_in.hready = '1' and ahbi_in.hgrant(0) = '1' and line_full = '0' then
	--			debug_data2		<= log & log2 & ahbi_in.hrdata;
	--			increment_addr2	<= std_logic_vector(to_unsigned(to_integer(unsigned(increment_addr2))+1,C_ADDR_DBUG));
	--			log2			<= std_logic_vector(to_unsigned(to_integer(unsigned(log2))+1, 16));
	--
	--		end if;
	--
	--	end if;
	--end process;

	line_buffer_rw : process (clk, rstn) is
		variable local_addres_v : natural;
		variable offset_v : std_logic_vector(1 downto 0);
		variable range_high_v : natural;
		variable range_low_v : natural;
		variable word_to_write_v : std_logic_vector(G_LINE_WIDTH downto 0);
	begin -- process line_buffer_rw
		if rstn = '0' then -- asynchronous reset (active low)
			ahbi_to_leon.hrdata <= (others => '0');
			--      ahbi_to_leon.hcache <= '0';
			local_addres_v := 0;
			line_buffer_wr_data <= (others => '0');
		elsif clk'event and clk = '1' then -- rising clock edge
			--transform the full physical data in a local data(to address the line buffer)
			local_addres_v := natural(to_integer(
				SHIFT_RIGHT(unsigned(master_address(C_BASE_ADDR_BITS_W - 1 downto 0)), 2)));
			offset_v := master_address(1 downto 0);
			-- write been performed by the read state
			if line_buffer_wr = '1' then
				asc_bufferr(asc_buffer_cur)(line_buffer_addr) <= line_buffer_data;
				--COMMENTED--			debug_data2		<= log & x"0000" & asc_bufferr(asc_buffer_cur)(line_buffer_addr)(32 downto 1);
				--COMMENTED--			increment_addr2	<= std_logic_vector(to_unsigned(to_integer(unsigned(increment_addr2))+1,C_ADDR_DBUG));
				log <= std_logic_vector(to_unsigned(to_integer(unsigned(log)) + 1, 16));
				--			debug_data3		<= log & x"0000" & asc_buffer_st(asc_buffer_cur).address(31 downto 5) & '0' & '0' & '0' & asc_buffer_st(asc_buffer_cur).secure & asc_buffer_hit;
				--COMMENTED--			debug_data3		<= log & x"0000" & ahbo_in.haddr;
				--COMMENTED--			increment_addr3	<= std_logic_vector(to_unsigned(to_integer(unsigned(increment_addr3))+1,C_ADDR_DBUG));
				debug_data <= log_in & x"A" & std_logic_vector(to_unsigned(line_buffer_addr, 4)) & std_logic_vector(to_unsigned(asc_buffer_cur, 4)) & x"0" & line_buffer_data(32 downto 1);
				debug_data2 <= log_in & x"A" & std_logic_vector(to_unsigned(line_buffer_addr, 4)) & std_logic_vector(to_unsigned(asc_buffer_cur, 4)) & x"0" & master_address;
				increment_addr <= std_logic_vector(to_unsigned(to_integer(unsigned(increment_addr)) + 1, C_ADDR_DBUG));
				increment_addr2 <= std_logic_vector(to_unsigned(to_integer(unsigned(increment_addr2)) + 1, C_ADDR_DBUG));

			end if;

			word_to_write_v := ahbo_in.hwdata & '1';

			--write been performed by the  processor
			if leon_write = '1' then

				range_high_v := 32;
				range_low_v := 1;
				if ahbo_in.hsize = HSIZE_BYTE then
					case offset_v is
						when "00" =>
							range_high_v := 32;
							range_low_v := 25;
						when "01" =>
							range_high_v := 24;
							range_low_v := 17;
						when "10" =>
							range_high_v := 16;
							range_low_v := 9;
						when "11" =>
							range_high_v := 8;
							range_low_v := 1;
						when others =>
							range_high_v := 32;
							range_low_v := 1;
					end case;
				elsif ahbo_in.hsize = HSIZE_HWORD then
					case offset_v is
						when "00" =>
							range_high_v := 32;
							range_low_v := 17;
						when "10" =>
							range_high_v := 16;
							range_low_v := 1;
						when others =>
							range_high_v := 32;
							range_low_v := 1;
					end case;
				end if;

				--atrriuitiion of the new data  already taking in account the byte or half writes
				asc_bufferr(asc_buffer_cur)
				(local_addres_v)(range_high_v downto range_low_v) <= word_to_write_v(range_high_v downto range_low_v);

				--if the data is cachable or not
				asc_bufferr(asc_buffer_cur)(local_addres_v)(0) <= '1';

				debug_data <= log_in & x"B" & std_logic_vector(to_unsigned(local_addres_v, 4)) & std_logic_vector(to_unsigned(asc_buffer_cur, 4)) & x"0" & word_to_write_v(32 downto 1);
				debug_data2 <= log_in & x"B" & std_logic_vector(to_unsigned(local_addres_v, 4)) & std_logic_vector(to_unsigned(asc_buffer_cur, 4)) & x"0" & master_address;
				increment_addr <= std_logic_vector(to_unsigned(to_integer(unsigned(increment_addr)) + 1, C_ADDR_DBUG));
				increment_addr2 <= std_logic_vector(to_unsigned(to_integer(unsigned(increment_addr2)) + 1, C_ADDR_DBUG));
			end if;

			--set the data to leon (normal read)
			ahbi_to_leon.hrdata <= asc_bufferr(asc_buffer_cur)(local_addres_v)(G_LINE_WIDTH downto 1);
			--      ahbi_to_leon.hcache <= asc_bufferr(asc_buffer_cur)(local_addres_v)(0);  -- cacheable

			-- thw write data need to be one cycle after  the address
			-- this logic certifies that it only changes when the previous addres has
			-- changed
			line_buffer_wr_data <= asc_bufferr(asc_buffer_cur)(addr_cnt)(G_LINE_WIDTH downto 1);
			if lf_state_reg = OUT_OF_RANGE then
				hwdata_to_bus <= ahbo_in.hwdata;
			elsif addr_cnt_en = '1' then
				if ahbo_to_bus.hwrite = '1' then
					debug_data3 <= log_in & x"0F" & std_logic_vector(to_unsigned(asc_buffer_cur, 4)) & x"0" & ahbo_to_bus.haddr;
					debug_data4 <= log_in & x"0F" & std_logic_vector(to_unsigned(asc_buffer_cur, 4)) & x"0" & ahbo_to_bus.hwdata;
				else
					debug_data3 <= log_in & x"00" & std_logic_vector(to_unsigned(asc_buffer_cur, 4)) & x"0" & ahbo_to_bus.haddr;
					debug_data4 <= log_in & x"00" & std_logic_vector(to_unsigned(asc_buffer_cur, 4)) & x"0" & ahbo_to_bus.hwdata;
				end if;
				increment_addr3 <= std_logic_vector(to_unsigned(to_integer(unsigned(increment_addr3)) + 1, C_ADDR_DBUG));
				increment_addr4 <= std_logic_vector(to_unsigned(to_integer(unsigned(increment_addr4)) + 1, C_ADDR_DBUG));
				hwdata_to_bus <= line_buffer_wr_data;
				if ahbi_in.hready = '1' then
					hwdata_to_bus <= asc_bufferr(asc_buffer_cur)(addr_cnt)(G_LINE_WIDTH downto 1);
				end if;
			end if;
		end if;
	end process line_buffer_rw;

	--The attribuition to hwdata must be combinational  since when the handler
	-- take control of the bus all signals are registered, this way the data is stable
	-- instantly and one cycle after the bus control
	process (lf_state_reg, ahbo_in.hwdata, hwdata_to_bus)
	begin
		if lf_state_reg = OUT_OF_RANGE then
			ahbo_to_bus.hwdata <= ahbo_in.hwdata;
		else
			ahbo_to_bus.hwdata <= hwdata_to_bus;
		end if;
	end process;

--COMMENTED--	debug_memory_inst4 : debug_memory PORT MAP (
--COMMENTED--			address	 => increment_addr4,
--COMMENTED--			clock	 => clk,
--COMMENTED--			data	 => debug_data4,
--COMMENTED--			wren	 => '1',
--COMMENTED--			q		 => open
--COMMENTED--		);
--COMMENTED--
--COMMENTED--	debug_memory_inst3 : debug_memory port map(
--COMMENTED--		address => increment_addr3,
--COMMENTED--		clock => clk,
--COMMENTED--		data => debug_data3,
--COMMENTED--		wren => '1',
--COMMENTED--		q => open
--COMMENTED--	);
--COMMENTED--
--COMMENTED--	debug_memory_inst2 : debug_memory port map(
--COMMENTED--		address => increment_addr2,
--COMMENTED--		clock => clk,
--COMMENTED--		data => debug_data2,
--COMMENTED--		wren => '1',
--COMMENTED--		q => open
--COMMENTED--	);
--COMMENTED--
--COMMENTED--	debug_memory_inst : debug_memory PORT MAP (
--COMMENTED--			address	 => increment_addr,
--COMMENTED--			clock	 => clk,
--COMMENTED--			data	 => debug_data,
--COMMENTED--			wren	 => '1',
--COMMENTED--			q		 => open
--COMMENTED--		);
	--COMMENTED--
	--COMMENTED--cache_debug_inst : cache_debug PORT MAP (
	--COMMENTED--		address		=> std_logic_vector(to_unsigned(write_address,8)),
	--COMMENTED--		clock		=> clk,
	--COMMENTED--		data		=> log_cache & x"0000000" & ptag_sreq.cache_line,
	--COMMENTED--		wren		=> '1',
	--COMMENTED--		q			=> open
	--COMMENTED--	);

end architecture;
