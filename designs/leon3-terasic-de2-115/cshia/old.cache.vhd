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
		discarding_addr		: out	std_logic_vector (TC_ADDR-1 downto 0);
		address				: in	std_logic_vector (TC_ADDR-1 downto 0);
		data_in				: in	std_logic_vector (C_RAM_WIDTH-1 downto 0);
		data_out			: out   std_logic_vector (C_RAM_WIDTH-1 downto 0)
	);
END ENTITY;

ARCHITECTURE rtl OF cache IS
	constant C_N_SETS				: natural := N_SETS;

	type	T_PTAG_BLOCK is array (C_N_SETS-1 downto 0) of T_WORD;
	type	T_BLOCK_ADDR is array (C_N_SETS-1 downto 0) of T_ADDRESS_VECTOR;

	type	T_STATUS_LINE is array (C_N_SETS-1 downto 0) of R_STATUS;
	type	T_STATUS_CACHE is array (C_RAM_DEPTH-1 downto 0) of T_STATUS_LINE;
	type	T_LRU_CACHE is array (C_RAM_DEPTH-1 downto 0) of natural;

	type	R_CACHE is record
			status					: T_STATUS_CACHE;
			lru						: T_LRU_CACHE;
	end record;

	signal	ptag_block_in,
   			ptag_block_out			: T_PTAG_BLOCK;

	signal	the_set					: natural range 0 to C_N_SETS-1;

	signal	the_cache				: R_CACHE;
	

	signal	block_addr 				: T_ADDRESS_VECTOR;
	signal	write_enable			: std_logic_vector (C_N_SETS-1 downto 0) := (others => '0'); 

	signal	wait_write,	
			was_hit					: std_logic;
	signal	the_tag					: std_logic_vector (TC_ADDR-C_LOG2_RAM_DEPTH-TC_LOG2_CHUNK-1 downto 0);
	
	signal	write_address			: integer range 0 to 255; 
	signal	log						: std_logic_vector(15 downto 0);

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

	Sets:
	for i in 0 to C_N_SETS-1 generate
		cache_sets:
		cache_set port map (
			clock1			=> clk,	
			clock2			=> clk,
			my_number		=> i,
			data			=> ptag_block_in(i),
			write_address	=> block_addr,
			read_address	=> block_addr,
			we				=> write_enable(i),
			q				=> ptag_block_out(i)
		);
	end generate;

	block_addr <= to_integer(unsigned(address(C_LOG2_RAM_DEPTH+TC_LOG2_CHUNK-1 downto TC_LOG2_CHUNK)));

	HitMiss:
	process (address, clk, rstn)
		variable	hit					: std_logic := '0';
		variable	l					: line;
		variable	has_invalid,
					has_non_dirty		: std_logic := '0';
		variable	lru					: natural := 0;
		variable	tag					: std_logic_vector (TC_ADDR-C_LOG2_RAM_DEPTH-TC_LOG2_CHUNK-1 downto 0);
		variable	c_addr				: natural range 0 to C_RAM_DEPTH-1;
		variable	set_to_change		: natural range 0 to C_N_SETS-1;
	begin
		c_addr			:= to_integer(unsigned(address(C_LOG2_RAM_DEPTH+TC_LOG2_CHUNK-1 downto TC_LOG2_CHUNK)));
		hit				:= '0';
		set_to_change	:= 0;
		has_invalid		:= '0';
		lru				:= 2**31-1;
		has_non_dirty	:= '0';
		tag				:= (others => '0');

		for i in 0 to C_N_SETS-1 loop
			if the_cache.status(c_addr)(i).tag = address(TC_ADDR-1 downto C_LOG2_RAM_DEPTH+TC_LOG2_CHUNK) and the_cache.status(c_addr)(i).valid = '1' then
				hit				:= '1';
				set_to_change	:= i;
				tag				:= the_cache.status(c_addr)(i).tag;
			end if;
		end loop;
		if hit = '0' then
			for i in 0 to C_N_SETS-1 loop
				if the_cache.status(c_addr)(i).valid /= '1' then
					has_invalid		:= '1';
					set_to_change	:= i;
					tag				:= the_cache.status(c_addr)(i).tag;
				end if;
			end loop;
		end if;
		if hit = '0' and has_invalid = '0' then
			for i in 0 to C_N_SETS-1 loop
-- CHTREE
				if the_cache.status(c_addr)(i).lru < lru then
					has_non_dirty	:= not the_cache.status(c_addr)(i).dirty;
					lru				:= the_cache.status(c_addr)(i).lru;
					set_to_change	:= i;
					tag				:= the_cache.status(c_addr)(i).tag;
				end if;
			end loop;
		end if;
-- SECBUS
--						if the_cache.status(c_addr)(i).dirty = '0' then
--							if (the_cache.status(c_addr)(i).lru < lru) or (has_non_dirty = '0') then
--								has_non_dirty	:= '1';
--								lru				:= the_cache.status(c_addr)(i).lru;
--								set_to_change	:= i;
--								tag				:= the_cache.status(c_addr)(i).tag;
--							end if;
--						elsif has_non_dirty = '0' then
--							if the_cache.status(c_addr)(i).lru < lru then
--								lru				:= the_cache.status(c_addr)(i).lru;
--								set_to_change	:= i;
--								tag				:= the_cache.status(c_addr)(i).tag;
--							end if;
--						end if;
--					end if;
--				end if;
--			end loop;

		the_set			<= set_to_change;
--		block_addr		<= c_addr;
		miss_rw			<= not hit;
--		was_hit			<= hit;
		hit_rw			<= hit; 
		the_tag			<= tag;
		discard			<= has_non_dirty or has_invalid; --read and (not hit); -- has_non_dirty or has_invalid; 

		if rstn = '0' then
			wait_write			<= '0';
			write_address		<= 0;

			for i in 0 to C_N_SETS-1 loop
				ptag_block_in (i)		<= (others => '0');
				write_enable (i)		<= '0';
				for j in 0 to C_RAM_DEPTH-1 loop
					the_cache.lru(j)				<= 0;
					the_cache.status(j)(i).lru		<= 0;
					the_cache.status(j)(i).tag		<= (others => '0');
					the_cache.status(j)(i).dirty	<= '0';
					the_cache.status(j)(i).valid	<= '0';
				end loop;
			end loop;
		elsif (rising_edge(clk)) then
			if wait_write = '1' then
				write_enable	<= (others => '0');

				wait_write		<= '0';
			elsif read = '1' or wrt = '1' then
				debug_data <= log & read & wrt & x"0" & '0' & '0' & the_cache.status(c_addr)(set_to_change).tag & the_cache.status(c_addr)(set_to_change).dirty & the_cache.status(c_addr)(set_to_change).valid & hit & set_invalid & std_logic_vector(to_unsigned(the_cache.status(c_addr)(set_to_change).lru, 12)) & '0' & '0' & address;
				increment_addr	<= std_logic_vector(to_unsigned(to_integer(unsigned(increment_addr))+1, C_ADDR_DBUG));

				if set_invalid = '1' then	
					the_cache.status(c_addr)(set_to_change).valid	<= '0';
				end if;

				if wrt = '1' then
					ptag_block_in (set_to_change)					<= data_in;
					the_cache.status(c_addr)(set_to_change).tag		<= address(TC_ADDR-1 downto C_LOG2_RAM_DEPTH+TC_LOG2_CHUNK);
					the_cache.status(c_addr)(set_to_change).dirty	<= dirtiness;
					the_cache.status(c_addr)(set_to_change).valid	<= '1';
					write_enable(set_to_change)						<= '1';
					write_address									<= write_address + 1;
					log												<= std_logic_vector(to_unsigned(to_integer(unsigned(log))+1, 16));
				end if;

				if (wrt = '1' or hit = '1') and verify = '0' then
					the_cache.lru(c_addr) 						<= the_cache.lru(c_addr) + 1;
					the_cache.status(c_addr)(set_to_change).lru	<= the_cache.lru(c_addr) + 1;
				end if;

				if wrt = '1' then
					wait_write	<= '1';
				end if;
			end if;
		end if; 
	end process;

	discarding_addr		<= the_tag & std_logic_vector(to_unsigned(block_addr, C_LOG2_RAM_DEPTH)) & std_logic_vector(to_unsigned(0, TC_LOG2_CHUNK));
	data_out			<= ptag_block_out (the_set);
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
