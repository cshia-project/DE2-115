library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
library CSHIA;
use CSHIA.handler_pkg.all;
use CSHIA.bch.all;
use CSHIA.ecc.all;
use CSHIA.apuf_pkg.all;

entity fuzzy_extractor is
	port (clk,
		rstn,
		reboot,
		enroll				: in  std_logic;
		key1				: OUT STD_LOGIC_VECTOR (C_KEY_LENGTH-1 DOWNTO 0);
		key2				: OUT STD_LOGIC_VECTOR (C_KEY_LENGTH-1 DOWNTO 0);
		done				: out std_logic
	);
end entity;

architecture rtl of fuzzy_extractor is

-------------------------------------------------------------------------------
------------------------------------ DEBUG ------------------------------------
-------------------------------------------------------------------------------
	signal	increment_addr	: std_logic_vector(10 downto 0);
	signal	debug_data		: std_logic_vector(63 downto 0);
	signal	log				: std_logic_vector(15 downto 0);
-------------------------------------------------------------------------------
------------------------------------ DEBUG ------------------------------------
-------------------------------------------------------------------------------

	constant C_KEY_PUFs			: integer := (4*(C_KEY_LENGTH/C_APUF_RESPONSE))/(C_KEY_LENGTH/RWORD_LENGTH);

	type T_STATES is (S_INIT, S_SETUP_R, S_SETUP_K, S_GETWORD, S_WAIT, S_NEXTWORD, S_GENERATION, S_REGENERATION, S_SETKEY, S_WAIT_AGAIN);
	type T_AUX_STATES is (S_KEY, S_RWORD);
	type T_CHALLENGE is array (C_KEY_PUFs-1 downto 0) of T_APUF_CHALLENGE;
	type T_RESPONSE is array (C_KEY_PUFs-1 downto 0) of T_APUF_RESPONSE;
	subtype T_CODEWORD is std_logic_vector (C_KEY_LENGTH-1 downto 0);
	type T_ALL_CODEWORD is array (C_KEY_PUFs-1 downto 0) of T_CODEWORD;

	signal	engine_enabled		: std_logic;

--	signal	key_challenges		: T_CHALLENGE := (x"01B4B7BFE6EEAD54", x"63E8FFAFCA35FE44", x"3116E138862395BE");
--	signal	rword_challenges	: T_CHALLENGE := (x"56884AE3880AE21A", x"599B9A31EE3104DA", x"8D232A886B646D6A");
	signal	key_challenges		: T_CHALLENGE := --(x"6A7F1538884F1D61",x"4508DACEB33EEF62",");
	(x"115684A0ED3CEBF3",x"04C2467134916834",x"22DAA7839B77CEED", x"10AFF6578183F8D4");
	signal	rword_challenges	: T_CHALLENGE := (x"FC98F2990A8ED1A6",x"96F33B85E012452E",x"3EC6221CC770A4D2",x"B742541A475410D7");

--	signal	key_challenges		: T_CHALLENGE := (x"0000000000000001", x"000000000000000A", x"0000000000000AAA");
--	signal	rword_challenges	: T_CHALLENGE := (x"1001000000010010", x"1000100000010010", x"0000000000010010");

	signal	keys, rwords,
			helpers				: T_RESPONSE;

	signal	helper_in,
			helper_out,
			rword,
			key,
			final_key			: T_APUF_RESPONSE;

	signal	state				: T_STATES;
	signal	aux_state			: T_AUX_STATES;
	signal	codewords			: T_ALL_CODEWORD;
	signal	engine_done			: std_logic;

	signal	challenge			: T_APUF_CHALLENGE;
	signal	responses			: T_APUF_RESPONSE;

	signal	clearn_pufs, clki,
			global_rst_fuzzy_n	: std_logic;
begin
--      
	clki <= clk;
	global_rst_fuzzy_n <= rstn and engine_enabled;
  
	Arbiter_PUF1:
	apuf  generic map (
		n_pufs				=> C_APUF_RESPONSE,
		n_stages			=> C_APUF_CHALLENGE 
	)
	port map (
		clk					=> clki,
		rst_n				=> rstn,
		clearn				=> clearn_pufs,
		challenge			=> challenge,
		responses			=> responses
	);
      
	FuzzyExtractor_ENROLL_REBOOT:
	fe_engine port map (
		clk					=> clki,
		rstn				=> global_rst_fuzzy_n,
		reboot				=> reboot,
		helper_data_dec		=> helper_in,
		extracted_rword		=> rword,
		extracted_key		=> key,
		recovered_key		=> final_key,
		helper_data_enc		=> helper_out,
		done				=> engine_done
	);
  
	Codewording:
	--PROCESS (clki, rstn, rword_addresses, key_addresses, sram_word, reboot)
	--WATCH LIST augusto
	process (clki, rstn)
		constant limit				: natural := 2*C_KEY_PUFs;
		variable counter			: natural range 0 to C_KEY_PUFs + 2;
		variable cycle_counter		: natural range 0 to 2*limit + 1;
	begin
		if rstn = '0' then
			counter				:= 0;
			cycle_counter		:= 0;
			key					<= (others => '0');
			key1				<= (others => '0');
			key2				<= (others => '0');
			rword				<= (others => '0');
			helper_in			<= (others => '0');
			state				<= S_INIT;
			done				<= '0';
			clearn_pufs			<= '0';
			engine_enabled		<= '0';
		elsif (RISING_EDGE(clki)) then
			case state is
			when S_INIT =>
				clearn_pufs			<= '1';
--COMMENTED--				debug_data			<= x"000000000000" & log;	
--COMMENTED--				increment_addr		<= std_logic_vector(to_unsigned(to_integer(unsigned(increment_addr))+1, 11));
--COMMENTED--				log					<= std_logic_vector(to_unsigned(to_integer(unsigned(log))+1, 16));
				state			<= S_SETUP_K;
--				limit			:= C_KEY_WORDS;
				aux_state		<= S_KEY;
			when S_SETUP_R =>
--COMMENTED--				debug_data			<= x"000000000000" & log;	
--COMMENTED--				increment_addr		<= std_logic_vector(to_unsigned(to_integer(unsigned(increment_addr))+1, 11));
--COMMENTED--				log					<= std_logic_vector(to_unsigned(to_integer(unsigned(log))+1, 16));
				counter			:= 0;
				cycle_counter	:= 0;
--				limit			:= C_R_WORDS;
				state			<= S_GETWORD;
			when S_SETUP_K =>
--COMMENTED--				debug_data			<= x"000000000000" & log;	
--COMMENTED--				increment_addr		<= std_logic_vector(to_unsigned(to_integer(unsigned(increment_addr))+1, 11));
--COMMENTED--				log					<= std_logic_vector(to_unsigned(to_integer(unsigned(log))+1, 16));
				counter			:= 0;
				cycle_counter	:= 0;
				state			<= S_GETWORD;
			when S_GETWORD =>
--				debug_data			<= x"000000000000" & log;	
--				increment_addr		<= std_logic_vector(to_unsigned(to_integer(unsigned(increment_addr))+1, 11));
--				log					<= std_logic_vector(to_unsigned(to_integer(unsigned(log))+1, 16));
				if aux_state = S_KEY then
					challenge	<= key_challenges(counter);
--COMMENTED--					debug_data			<= key_challenges(counter);
--COMMENTED--					increment_addr		<= std_logic_vector(to_unsigned(to_integer(unsigned(increment_addr))+1, 11));
				else
					-- Syndrome
					challenge	<= rword_challenges(counter);
--COMMENTED--					debug_data			<= rword_challenges(counter);
--COMMENTED--					increment_addr		<= std_logic_vector(to_unsigned(to_integer(unsigned(increment_addr))+1, 11));
				end if;
				state <= S_WAIT;
			when S_WAIT =>
--				clearn_pufs		<= '1';
				cycle_counter	:= cycle_counter + 1;
				if cycle_counter =	limit then
					cycle_counter := 0;
					state <= S_NEXTWORD;
				 end if;
				if aux_state = S_KEY then
					keys(counter)		<= responses;
--COMMENTED--					debug_data			<= responses;
--COMMENTED--					increment_addr		<= std_logic_vector(to_unsigned(to_integer(unsigned(increment_addr))+1, 11));
				else
					rwords(counter)		<= responses;
--COMMENTED--					debug_data			<= responses;
--COMMENTED--					increment_addr		<= std_logic_vector(to_unsigned(to_integer(unsigned(increment_addr))+1, 11));
				end if;
			when S_NEXTWORD =>
				counter := counter + 1;
--COMMENTED--				debug_data			<= x"000000000000" & log;
--COMMENTED--				increment_addr		<= std_logic_vector(to_unsigned(to_integer(unsigned(increment_addr))+1, 11));
--COMMENTED--				log					<= std_logic_vector(to_unsigned(to_integer(unsigned(log))+1, 16));
				if counter >= C_KEY_PUFs then
					if (aux_state = S_KEY) then
						aux_state	<= S_RWORD;
						state		<= S_SETUP_R;
					elsif (reboot = '1') then
						state <= S_REGENERATION;
					else
						state <= S_GENERATION;
					end if;
					counter := 0;
				else
					state <= S_GETWORD;
				end if;
			when S_GENERATION =>
				engine_enabled		<= '0';
				key					<= keys(counter);
				rword				<= rwords(counter);
				
				state				<= S_WAIT_AGAIN;
--COMMENTED--				debug_data			<= x"000000000000" & log;
--COMMENTED--				increment_addr		<= std_logic_vector(to_unsigned(to_integer(unsigned(increment_addr))+1, 11));
--COMMENTED--				log					<= std_logic_vector(to_unsigned(to_integer(unsigned(log))+1, 16));
				
			when S_REGENERATION =>
				engine_enabled		<= '0';
				key					<= keys(counter);
				rword				<= rwords(counter);
				helper_in			<= helpers(counter);
				
				state				<= S_WAIT_AGAIN;
--COMMENTED--				debug_data			<= x"000000000000" & log;
--COMMENTED--				increment_addr		<= std_logic_vector(to_unsigned(to_integer(unsigned(increment_addr))+1, 11));
--COMMENTED--				log					<= std_logic_vector(to_unsigned(to_integer(unsigned(log))+1, 16));
			when S_WAIT_AGAIN =>
				engine_enabled		<= '1';
				if engine_done = '1' then
					if (reboot = '1') then
						keys(counter)		<= final_key;
						state				<= S_REGENERATION;
						counter 			:= counter + 1;
--COMMENTED--						debug_data			<= final_key;
--COMMENTED--						increment_addr		<= std_logic_vector(to_unsigned(to_integer(unsigned(increment_addr))+1, 11));
					else
						helpers(counter)	<= helper_out;
						state				<= S_GENERATION;
						counter 			:= counter + 1;
--COMMENTED--						debug_data			<= helper_out;
--COMMENTED--						increment_addr		<= std_logic_vector(to_unsigned(to_integer(unsigned(increment_addr))+1, 11));
					end if;
					if counter >= C_KEY_PUFs then
						state				<= S_SETKEY;
						counter 			:= 0;
					end if;
				end if;
			when others =>
				engine_enabled	<= '0';
				done			<= '1';
				key1			<=	keys(1) & keys(0);
				key2			<=	keys(3) & keys(2);
			end case;
		end if;
	end process;

--COMMENTED--	debug_memory_inst : debug_memory PORT MAP (
--COMMENTED--			address	 => increment_addr,
--COMMENTED--			clock	 => clk,
--COMMENTED--			data	 => debug_data,
--COMMENTED--			wren	 => '1',
--COMMENTED--			q		 => open
--COMMENTED--		);

end architecture;
