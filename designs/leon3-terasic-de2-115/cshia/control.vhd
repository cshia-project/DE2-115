LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.numeric_std.ALL;
LIBRARY CSHIA;
USE CSHIA.cache_pkg.ALL;
USE CSHIA.tree_pkg.ALL;
use CSHIA.DBUG_PKG.ALL;

USE std.textio.ALL;

ENTITY control IS
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
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
		root_level			: in	natural range 0 to TC_DEPTH;
		bus_address			: in	std_logic_vector (TC_ADDR-1 downto 0);
		requested_address	: out	std_logic_vector (TC_ADDR-1 downto 0);
		returning_address	: out	std_logic_vector (TC_ADDR-1 downto 0);
		data_in				: in	std_logic_vector (C_RAM_WIDTH-1 downto 0);
		data_out			: out   std_logic_vector (C_RAM_WIDTH-1 downto 0)
	);
END ENTITY;

ARCHITECTURE rtl OF control IS
-------------------------------------------------------------------------------
------------------------------------ DEBUG ------------------------------------
-------------------------------------------------------------------------------
	signal	increment_addr	: std_logic_vector(C_ADDR_DBUG-1 downto 0);
	signal	debug_data		: std_logic_vector(63 downto 0);
	signal	log				: std_logic_vector(15 downto 0);
-------------------------------------------------------------------------------
------------------------------------ DEBUG ------------------------------------
-------------------------------------------------------------------------------

	type T_STATES is (S_INIT, S_START, S_CHECK, S_READ_ADDR, S_SET_ORIGINAL_ADDR, S_JUST_BRING, S_WAIT, S_VISIT_ANCESTOR, S_FATHER);

	constant C_N_SETS		: natural := N_SETS;
	constant C_SHIFT_DEPTH	: std_logic_vector (TC_LOG2_DEPTH-1 downto 0) := (others => '0');
	constant C_SHIFT_CHUNK	: std_logic_vector (TC_LOG2_CHUNK-1 downto 0) := (others => '0');

	signal	write_data,
			just_verify,
			miss,
			hit,
			read_data,
			set_invalid,
			discard			: std_logic;
	signal	final_addr,
			discarding_addr	: std_logic_vector (TC_ADDR-1 downto 0);
	signal	input_data,
			output_data		: std_logic_vector (C_RAM_WIDTH-1 downto 0);

	signal	states			: T_STATES;
BEGIN

	Inst_CacheComponent:
	cache generic map (
		C_N_SETS
	) port map (
		clk				=> clk,
		rstn			=> rstn,
		dirtiness		=> dirtiness,
		set_invalid		=> set_invalid,
		verify			=> just_verify,
		read			=> read_data,
		wrt				=> write_data,
		miss_rw			=> miss,
		discard			=> discard,
		hit_rw			=> hit,
		address			=> final_addr,
		discarding_addr	=> discarding_addr,
		data_in			=> input_data,
		data_out		=> output_data	
	);

--	final_addr			<= C_SHIFT_CHUNK & addr(TC_ADDR-1 downto TC_LOG2_CHUNK);

	Control_StateMachine:
	process (clk, rstn)
		variable	reg_out, reg_in,
					original_reg	: std_logic_vector (C_RAM_WIDTH-1 downto 0);
		variable	original_addr,
					addr			: std_logic_vector (TC_ADDR-1 downto 0);
		variable	original_cmd,
					cmd				: std_logic;
		variable	father_level	: integer range 0 to TC_DEPTH;
		variable	father_addr		: std_logic_vector (TC_ADDR-1 downto 0);
		variable	write_enable	: std_logic;
		variable	finish_queue	: boolean;

		variable	l				: line;
		variable	read_enable		: std_logic;
	begin
		if (rstn = '0') then
			read_data			<= '0';
			write_data			<= '0';
			requesting			<= '0';
			sending				<= '0';
			set_invalid			<= '0';
			ready_to_receive	<= '0';

--			data_out			<= (others => '0');
--			input_data			<= (others => '0');
			requested_address	<= (others => '0');
			returning_address	<= (others => '0');
			final_addr			<= (others => '0');

			original_addr		:= (others => '0');
			original_reg		:= (others => '0');
			original_cmd		:= '0';

			write_enable		:= '0';
			read_enable			:= '0';
			addr				:= (others => '0');
			reg_out				:= (others => '0');
			reg_in				:= (others => '0');
			father_addr			:= (others => '0');
			father_level		:= 0;
			finish_queue		:= false;

			states				<= S_INIT;
		elsif (rising_edge(clk)) then
			case states is
			when S_INIT =>
				write_enable		:= '0';
				read_enable			:= '0';
				set_invalid			<= '0';
				just_verify			<= '0';
				requesting			<= '0';
				sending				<= '0';
				ready_to_receive	<= '0';
				states				<= S_START;
				debug_data			<= x"FFFFFFFF0000" & log;
				increment_addr		<= std_logic_vector(to_unsigned(to_integer(unsigned(increment_addr))+1,C_ADDR_DBUG));
				log					<= std_logic_vector(to_unsigned(to_integer(unsigned(log))+1, 16));
			when S_START =>
				ready_to_receive	<= '1';
				if incoming_ready = '1' then
					addr				:= bus_address;
--					reg_out				:= output_data;
--					reg_in				:= data_in;
--					write_enable		:= wrt;
--					read_enable			:= not wrt;
					write_enable		:= '0';
					read_enable			:= '1';
					ready_to_receive	<= '0';
					states				<= S_CHECK;
				end if;
			when S_CHECK =>
--				just_verify	<= '0';
				states			<= S_INIT;
				write_enable	:= wrt and hit;
				read_enable		:= '0';

--COMMENTED--				debug_data		<= x"B" & write_enable & dirtiness & discard & miss & x"000" & '0' & '0' & discarding_addr & x"000" & '0' & '0' & addr;
--COMMENTED--				increment_addr	<= std_logic_vector(to_unsigned(to_integer(unsigned(increment_addr))+1,C_ADDR_DBUG));
				if miss = '1' then
					requested_address	<= addr;
--					write_enable		:= '0';
					read_enable			:= '1';

					original_addr		:= addr;
					if discard = '1' then
--						requesting	<= '1';
						just_verify			<= '1';
						father_addr			:= find_next_address(addr);
						father_level		:= to_integer(unsigned(father_addr(TC_ADDR-2 downto TC_ADDR-1-TC_LOG2_DEPTH)));
						-- Setup ancestors verification
--COMMENTED--						if addr(TC_ADDR-1) = '1' then
--COMMENTED--							father_level	:= to_integer(unsigned(addr(TC_ADDR-2 downto TC_ADDR-1-TC_LOG2_DEPTH)))+1;
--COMMENTED--							father_addr		:= '1' & std_logic_vector(to_unsigned(father_level, TC_LOG2_DEPTH)) & C_SHIFT_CHUNK & addr(TC_ADDR-2-TC_LOG2_DEPTH downto TC_LOG2_CHUNK);
--COMMENTED--						else
--COMMENTED--							father_level	:= 1;
--COMMENTED--							father_addr		:= '1' & std_logic_vector(to_unsigned(father_level, TC_LOG2_DEPTH)) & addr(TC_ADDR-TC_LOG2_DEPTH downto TC_LOG2_CHUNK);
--COMMENTED--						end if;
						addr				:= father_addr;
						states				<= S_VISIT_ANCESTOR;
					else
						addr				:= discarding_addr;
						returning_address	<= discarding_addr;
						set_invalid			<= '1';
						states				<= S_READ_ADDR;
					end if;
				end if;
			when S_VISIT_ANCESTOR =>
				read_enable			:= '0';
				just_verify			<= '0';
--				debug_data			<= '0' & '1' & miss & '0' & x"0" & '0' & '0' & original_addr & std_logic_vector(to_unsigned(0, 64-2*TC_ADDR-10)) & father_addr;
--				increment_addr		<= std_logic_vector(to_unsigned(to_integer(unsigned(increment_addr))+1,C_ADDR_DBUG));
				if ready = '1' then
					if miss = '1' then
						no_ancestor <= '1';
					else
						no_ancestor <= '0';
					end if;
					returning_address	<= father_addr;
					requesting			<= '1';
					states				<= S_SET_ORIGINAL_ADDR;
--					debug_data			<= '1' & '0' & miss & '1' & original_addr & std_logic_vector(to_unsigned(0, 64-2*TC_ADDR-4)) & father_addr;
--					increment_addr		<= std_logic_vector(to_unsigned(to_integer(unsigned(increment_addr))+1,C_ADDR_DBUG));
--COMMENTED--					debug_data			<= '0' & '1' & miss & discard & x"0" & '0' & '0' & original_addr & std_logic_vector(to_unsigned(0, 64-2*TC_ADDR-10)) & father_addr;
--COMMENTED--					increment_addr		<= std_logic_vector(to_unsigned(to_integer(unsigned(increment_addr))+1,C_ADDR_DBUG));
				end if;
			when S_SET_ORIGINAL_ADDR =>
				addr				:= original_addr;
--				just_verify		<= '1';

				read_enable			:= '1';
				requesting			<= '0';
				states				<= S_JUST_BRING;	
			when S_JUST_BRING =>
				read_enable		:= '0';
--				just_verify	<= '0';

				if incoming_ready = '1' then
					reg_in			:= data_in;
					write_enable	:= '1';
					states			<= S_WAIT;
--COMMENTED--					debug_data		<= x"D" & '0' & dirtiness & '0' & '0' & std_logic_vector(to_unsigned(0, 64-TC_ADDR-8)) & addr;
--COMMENTED--					increment_addr	<= std_logic_vector(to_unsigned(to_integer(unsigned(increment_addr))+1,C_ADDR_DBUG));
				end if;
			when S_WAIT =>
				write_enable	:= '0';
				states			<= S_FATHER;	
			when S_FATHER =>
				states			<= S_CHECK;
				just_verify		<= '1';
				read_enable		:= '1';

				father_addr			:= find_next_address(addr);
				father_level		:= to_integer(unsigned(father_addr(TC_ADDR-2 downto TC_ADDR-1-TC_LOG2_DEPTH)));
--COMMENTED--				if addr(TC_ADDR-1) = '1' then
--COMMENTED--					father_level	:= to_integer(unsigned(addr(TC_ADDR-2 downto TC_ADDR-1-TC_LOG2_DEPTH)))+1;
--COMMENTED--					father_addr		:= '1' & std_logic_vector(to_unsigned(father_level, TC_LOG2_DEPTH)) & C_SHIFT_CHUNK & addr(TC_ADDR-2-TC_LOG2_DEPTH downto TC_LOG2_CHUNK);
--COMMENTED--				else
--COMMENTED--					father_level	:= 1;
--COMMENTED--					father_addr		:= '1' & std_logic_vector(to_unsigned(father_level, TC_LOG2_DEPTH)) & addr(TC_ADDR-TC_LOG2_DEPTH downto TC_LOG2_CHUNK);
--COMMENTED--				end if;
--COMMENTED--				debug_data			<= '0' & '0' & father_addr & x"00" & std_logic_vector(to_unsigned(father_level,8)) & std_logic_vector(to_unsigned(root_level, 8)) & std_logic_vector(to_unsigned(0, 64-2*TC_ADDR-26)) & addr;
--COMMENTED--				increment_addr		<= std_logic_vector(to_unsigned(to_integer(unsigned(increment_addr))+1,C_ADDR_DBUG));
				addr			:= father_addr;
				if father_level >= root_level then
					read_enable		:= '0';
					finish_queue	:= false;
					--ptag_address	<= father_addr;
					just_verify		<= '0';
					states			<= S_INIT;
				end if;
			when S_READ_ADDR =>
				read_enable			:= '0';
				addr				:= original_addr;
				set_invalid			<= '0';
				if ready = '1' then
					sending				<= '1';
--COMMENTED--					debug_data			<= x"C" & miss  & discard & '1' & '1' & '0' & '0' & addr & std_logic_vector(to_unsigned(0, 64-2*TC_ADDR-10)) & discarding_addr;
--COMMENTED--					increment_addr		<= std_logic_vector(to_unsigned(to_integer(unsigned(increment_addr))+1,C_ADDR_DBUG));

--				states		<= S_SEND_BACK;
--			when S_SEND_BACK =>
--				reg_out				:= output_data;
--				returning_address	<= discarding_addr; 
--				sending				<= '1';

					states				<= S_INIT;
				end if;
--				debug_data			<= '1' & '1' & addr & std_logic_vector(to_unsigned(0, 64-2*TC_ADDR-2)) & discarding_addr;
--				increment_addr		<= std_logic_vector(to_unsigned(to_integer(unsigned(increment_addr))+1,C_ADDR_DBUG));
			end case;
			final_addr	<= addr;
			write_data	<= write_enable;
			read_data	<= read_enable;
		end if;
	end process Control_StateMachine;
	input_data	<= data_in;	
	data_out	<= output_data;

--COMMENTED--	debug_memory_inst : debug_memory port map (
--COMMENTED--			address	 => increment_addr,
--COMMENTED--			clock	 => clk,
--COMMENTED--			data	 => debug_data,
--COMMENTED--			wren	 => '1',
--COMMENTED--			q		 => open
--COMMENTED--		);
END ARCHITECTURE;
