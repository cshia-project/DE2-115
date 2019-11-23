LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.numeric_std.ALL;
LIBRARY CSHIA;
USE CSHIA.cache_pkg.ALL;
USE CSHIA.tree_pkg.ALL;
use CSHIA.DBUG_PKG.all;

USE std.textio.ALL;

ENTITY cache IS
	GENERIC (
		N_SETS				: in natural range 1 to 32 := 16
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
		discarding_addr		: out	std_logic_vector (C_PTAG_ADDR-1 downto 0);
		address				: in	std_logic_vector (C_PTAG_ADDR-1 downto 0);
		data_in				: in	std_logic_vector (C_RAM_WIDTH-1 downto 0);
		data_out			: out   std_logic_vector (C_RAM_WIDTH-1 downto 0)
	);
END ENTITY;

ARCHITECTURE rtl OF cache IS
	constant C_N_SETS				: natural := N_SETS/2;

	type	T_PTAG_BLOCK is array (C_N_SETS-1 downto 0) of T_WORD;
	type	T_BLOCK_ADDR is array (C_N_SETS-1 downto 0) of T_ADDRESS_VECTOR;

	type	T_STATUS_LINE is array (C_N_SETS-1 downto 0) of R_STATUS;
	type	T_STATUS_CACHE is array (C_RAM_DEPTH-1 downto 0) of T_STATUS_LINE;
	type	T_LRU_CACHE is array (C_RAM_DEPTH-1 downto 0) of natural;

	type	R_CACHE is record
			status					: T_STATUS_CACHE;
			lru						: T_LRU_CACHE;
	end record;

	signal	ptag_block_out1,
   			ptag_block_out2			: T_PTAG_BLOCK;

	signal	the_set					: natural range 0 to C_N_SETS-1;
	signal	the_choice				: natural;

	signal	the_cache1,
			the_cache2				: R_CACHE;

	signal	block_addr 				: T_ADDRESS_VECTOR;
	signal	write_enable1,
			write_enable2			: std_logic_vector (C_N_SETS-1 downto 0) := (others => '0'); 

	signal	wait_write,	
			was_hit					: std_logic;
	signal	the_tag					: std_logic_vector (C_PTAG_ADDR-C_LOG2_RAM_DEPTH-TC_LOG2_CHUNK-1 downto 0);
	
	signal	write_address			: integer range 0 to 255; 
	signal	log						: std_logic_vector(15 downto 0);

	signal	lrus1, lrus2			: T_LRU_VEC;

-------------------------------------------------------------------------------
------------------------------------ DEBUG ------------------------------------
-------------------------------------------------------------------------------
	signal	increment_addr2	: std_logic_vector(C_ADDR_DBUG-1 downto 0);
	signal	debug_data2		: std_logic_vector(63 downto 0);
	signal	log2			: std_logic_vector(15 downto 0);

	signal	increment_addr	: std_logic_vector(C_ADDR_DBUG-1 downto 0);
	signal	debug_data		: std_logic_vector(63 downto 0);
	--signal	log2			: std_logic_vector(15 downto 0);
-------------------------------------------------------------------------------
------------------------------------ DEBUG ------------------------------------
-------------------------------------------------------------------------------
BEGIN

	Sets1:
	for i in 0 to C_N_SETS-1 generate
		cache_sets:
		cache_set port map (
			clock1			=> clk,	
			clock2			=> clk,
			my_number		=> i,
			data			=> data_in,
			write_address	=> block_addr,
			read_address	=> block_addr,
			we				=> write_enable1(i),
			q				=> ptag_block_out1(i)
		);
	end generate;

	Sets2:
	for i in 0 to C_N_SETS-1 generate
		cache_sets:
		cache_set port map (
			clock1			=> clk,	
			clock2			=> clk,
			my_number		=> i,
			data			=> data_in, 
			write_address	=> block_addr,
			read_address	=> block_addr,
			we				=> write_enable2(i),
			q				=> ptag_block_out2(i)
		);
	end generate;

	block_addr <= to_integer(unsigned(address(C_LOG2_RAM_DEPTH+TC_LOG2_CHUNK-1 downto TC_LOG2_CHUNK)));

	HitMiss:
	process (address, clk, rstn, the_cache1, the_cache2)
		variable	hit1, hit2			: std_logic := '0';
		variable	l					: line;
		variable	has_invalid1,
					has_invalid2,
					has_non_dirty1,
					has_non_dirty2		: std_logic := '0';
		variable	lru1, lru2			: natural := 0;
		variable	tag					: std_logic_vector (C_PTAG_ADDR-C_LOG2_RAM_DEPTH-TC_LOG2_CHUNK-1 downto 0);
		variable	c_addr				: natural range 0 to C_RAM_DEPTH-1;
		variable	set1a, set1b,
					set2a, set2b,
					set3a, set3b,
					chosen_set			: natural range 0 to C_N_SETS-1;
		variable	chosen_number		: natural;
	begin
		c_addr			:= to_integer(unsigned(address(C_LOG2_RAM_DEPTH+TC_LOG2_CHUNK-1 downto TC_LOG2_CHUNK)));
		hit1			:= '0';
		hit2			:= '0';
		set1a			:= 0;
		set2a			:= 0;
		set3a			:= 0;
		set1b			:= 0;
		set2b			:= 0;
		set3b			:= 0;
		chosen_set		:= 0;
		has_invalid1	:= '0';
		has_invalid2	:= '0';
		lru1			:= the_cache1.lru(c_addr) + 2;
		lru2			:= the_cache2.lru(c_addr) + 2;
		has_non_dirty1	:= '0';
		has_non_dirty2	:= '0';
		tag				:= (others => '0');
		chosen_number	:= 0;

		for i in 0 to C_N_SETS-1 loop
			if the_cache1.status(c_addr)(i).tag = address(C_PTAG_ADDR-1 downto C_LOG2_RAM_DEPTH+TC_LOG2_CHUNK) and the_cache1.status(c_addr)(i).valid = '1' then
				hit1			:= '1';
				set1a			:= i;
			end if;
			if the_cache1.status(c_addr)(i).valid /= '1' then
				has_invalid1	:= '1';
				set2a			:= i;
			end if;
			if the_cache1.status(c_addr)(i).lru < lru1 then
				has_non_dirty1	:= not the_cache1.status(c_addr)(i).dirty;
				lru1			:= the_cache1.status(c_addr)(i).lru;
				set3a			:= i;
			end if;
		end loop;
		for i in 0 to C_N_SETS-1 loop
			if the_cache2.status(c_addr)(i).tag = address(C_PTAG_ADDR-1 downto C_LOG2_RAM_DEPTH+TC_LOG2_CHUNK) and the_cache2.status(c_addr)(i).valid = '1' then
				hit2			:= '1';
				set1b			:= i;
			end if;
			if the_cache2.status(c_addr)(i).valid /= '1' then
				has_invalid2	:= '1';
				set2b			:= i;
			end if;
			if the_cache2.status(c_addr)(i).lru < lru2 then
				has_non_dirty2	:= not the_cache2.status(c_addr)(i).dirty;
				lru2			:= the_cache2.status(c_addr)(i).lru;
				set3b			:= i;
			end if;
		end loop;

		if hit1 = '1' then
			chosen_set		:= set1a;
			tag				:= the_cache1.status(c_addr)(set1a).tag;
			chosen_number	:= 1;
		elsif hit2 = '1' then
			chosen_set		:= set1b;
			tag				:= the_cache2.status(c_addr)(set1b).tag;
			chosen_number	:= 2;
		elsif has_invalid1 = '1' then
			chosen_set		:= set2a;
			tag				:= the_cache1.status(c_addr)(set2a).tag;
			chosen_number	:= 1;
		elsif has_invalid2 = '1' then
			chosen_set		:= set2b;
			tag				:= the_cache2.status(c_addr)(set2b).tag;
			chosen_number	:= 2;
		elsif lru1 < lru2 then
			chosen_set		:= set3a;
			tag				:= the_cache2.status(c_addr)(set3a).tag;
			chosen_number	:= 1;
		else
			chosen_set		:= set3b;
			tag				:= the_cache2.status(c_addr)(set3b).tag;
			chosen_number	:= 2;
		end if;

		the_choice		<= chosen_number;
		the_set			<= chosen_set;
--		block_addr		<= c_addr;
		miss_rw			<= (not hit1) and (not hit2);
--		was_hit			<= hit;
		hit_rw			<= hit1 or hit2; 
		the_tag			<= tag;
		discard			<= has_non_dirty1 or has_non_dirty2 or has_invalid1 or has_invalid2; --read and (not hit); -- has_non_dirty or has_invalid; 

		if rstn = '0' then
			wait_write			<= '0';
			write_address		<= 0;

			for i in 0 to C_N_SETS-1 loop
				write_enable1(i)		<= '0';
				write_enable2(i)		<= '0';
				for j in 0 to C_RAM_DEPTH-1 loop
					the_cache1.lru(j)				<= 0;
					the_cache2.lru(j)				<= 0;

					the_cache1.status(j)(i).lru		<= 0;
					the_cache1.status(j)(i).tag		<= (others => '0');
					the_cache1.status(j)(i).dirty	<= '0';
					the_cache1.status(j)(i).valid	<= '0';
					the_cache2.status(j)(i).lru		<= 0;
					the_cache2.status(j)(i).tag		<= (others => '0');
					the_cache2.status(j)(i).dirty	<= '0';
					the_cache2.status(j)(i).valid	<= '0';
				end loop;
			end loop;
		elsif (rising_edge(clk)) then
			if wait_write = '1' then
				write_enable1	<= (others => '0');
				write_enable2	<= (others => '0');

				wait_write		<= '0';
			elsif read = '1' or wrt = '1' then
				if chosen_number = 1 then
--COMMENTED--					debug_data <= log & read & wrt & x"0" & '0' & '0' & the_cache1.status(c_addr)(chosen_set).tag & the_cache1.status(c_addr)(chosen_set).dirty & the_cache1.status(c_addr)(chosen_set).valid & hit1 & set_invalid & std_logic_vector(to_unsigned(the_cache1.status(c_addr)(chosen_set).lru, 12)) & '0' & '0' & address;
				else
--COMMENTED--					debug_data <= log & read & wrt & x"0" & '0' & '0' & the_cache2.status(c_addr)(chosen_set).tag & the_cache2.status(c_addr)(chosen_set).dirty & the_cache2.status(c_addr)(chosen_set).valid & hit2 & set_invalid & std_logic_vector(to_unsigned(the_cache2.status(c_addr)(chosen_set).lru, 12)) & '0' & '0' & address;
				end if;
				increment_addr	<= std_logic_vector(to_unsigned(to_integer(unsigned(increment_addr))+1, C_ADDR_DBUG));

				if set_invalid = '1' then	
					if chosen_number = 1 then
						the_cache1.status(c_addr)(chosen_set).valid	<= '0';
					else
						the_cache2.status(c_addr)(chosen_set).valid	<= '0';
					end if;
				end if;

				if wrt = '1' then
					if chosen_number = 1 then
						the_cache1.status(c_addr)(chosen_set).tag		<= address(C_PTAG_ADDR-1 downto C_LOG2_RAM_DEPTH+TC_LOG2_CHUNK);
						the_cache1.status(c_addr)(chosen_set).dirty		<= dirtiness;
						the_cache1.status(c_addr)(chosen_set).valid		<= '1';
						write_enable1(chosen_set)						<= '1';
					else
						the_cache2.status(c_addr)(chosen_set).tag		<= address(C_PTAG_ADDR-1 downto C_LOG2_RAM_DEPTH+TC_LOG2_CHUNK);
						the_cache2.status(c_addr)(chosen_set).dirty		<= dirtiness;
						the_cache2.status(c_addr)(chosen_set).valid		<= '1';
						write_enable2(chosen_set)						<= '1';
					end if;
					write_address									<= write_address + 1;
					log												<= std_logic_vector(to_unsigned(to_integer(unsigned(log))+1, 16));
				end if;

				if (wrt = '1' or hit1 = '1' or hit2 = '1') and verify = '0' then
					the_cache2.lru(c_addr)	<= the_cache2.lru(c_addr) + 1;
					the_cache1.lru(c_addr)	<= the_cache1.lru(c_addr) + 1;
					if chosen_number = 1 then
						the_cache1.status(c_addr)(chosen_set).lru	<= the_cache1.lru(c_addr) + 1;
					else
						the_cache2.status(c_addr)(chosen_set).lru	<= the_cache2.lru(c_addr) + 1;
					end if;
				end if;

				if wrt = '1' then
					wait_write	<= '1';
				end if;
			end if;
		end if; 
	end process;

	discarding_addr		<= the_tag & std_logic_vector(to_unsigned(block_addr, C_LOG2_RAM_DEPTH)) & std_logic_vector(to_unsigned(0, TC_LOG2_CHUNK));
	data_out			<= ptag_block_out1 (the_set) when the_choice = 1 else ptag_block_out2 (the_set);
--	hit_rw				<= was_hit;

--COMMENTED--cache_debug_inst : cache_debug PORT MAP (
--COMMENTED--		address		=> std_logic_vector(to_unsigned(write_address,8)),
--COMMENTED--		clock		=> clk,
--COMMENTED--		data		=> log & '0' & '0' & address & std_logic_vector(to_unsigned(block_addr, 2)) & std_logic_vector(to_unsigned(the_set,2)) & x"0" & dirtiness & '0' & '0' & '0' & data_in,
--COMMENTED--		wren		=> wrt,
--COMMENTED--		q			=> open
--COMMENTED--	);

--COMMENTED--	debug_memory_inst19 : debug_memory port map (
--COMMENTED--			address	 => increment_addr,
--COMMENTED--			clock	 => clk, 
--COMMENTED--			data	 => debug_data,
--COMMENTED--			wren	 => '1', 
--COMMENTED--			q		 => open
--COMMENTED--		);
END ARCHITECTURE;
