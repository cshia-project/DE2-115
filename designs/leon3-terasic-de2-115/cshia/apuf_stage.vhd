library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

entity apuf_stage is
	port (
		in1, in2				: in STD_LOGIC;
		path					: in STD_LOGIC_VECTOR (1 downto 0);
		out1, out2				: out STD_LOGIC
	);
end entity;

architecture RTL of apuf_stage is
	attribute keep : STRING;
	
	signal	b_p1,c_p1,d_p1,e_p1,f_p1,
			b_p2,c_p2,d_p2,e_p2,f_p2,
			a_p3,b_p3,c_p3,d_p3,e_p3,f_p3,
			a_p4,b_p4,c_p4,d_p4,e_p4,f_p4				: STD_LOGIC;
	
	attribute keep of	b_p1,c_p1,d_p1,e_p1,f_p1,
						b_p2,c_p2,d_p2,e_p2,f_p2,
						a_p3,b_p3,c_p3,d_p3,e_p3,f_p3,
						a_p4,b_p4,c_p4,d_p4,e_p4,f_p4	: signal is "true";
begin
	f_p1 <= (e_p1 and in1) or (d_p1 and in1) or (c_p1 and in1) or (b_p1 and in1) or ((not b_p1) and in1);
	f_p2 <= (e_p2 and in2) or (d_p2 and in2) or (c_p2 and in2) or (b_p2 and in2) or ((not b_p2) and in2);

	a_p3 <= f_p1 when path(0) = '0' else f_p2;
	a_p4 <= f_p2 when path(0) = '0' else f_p1;

	f_p3 <= (e_p3 and a_p3) or (d_p3 and a_p3) or (c_p3 and a_p3) or (b_p3 and a_p3) or ((not b_p3) and a_p3);
	f_p4 <= (e_p4 and a_p4) or (d_p4 and a_p4) or (c_p4 and a_p4) or (b_p4 and a_p4) or ((not b_p4) and a_p4);

	out1 <= f_p3 when path(1) = '0' else f_p4;
	out2 <= f_p4 when path(1) = '0' else f_p3;

--	b_p1 <= path(0);
--	c_p1 <= path(0);
--	d_p1 <= path(0);
--	e_p1 <= path(0);
--	b_p2 <= path(0);
--	c_p2 <= path(0);
--	d_p2 <= path(0);
--	e_p2 <= path(0);
--	b_p3 <= path(0);
--	c_p3 <= path(0);
--	d_p3 <= path(0);
--	e_p3 <= path(0);
--	b_p4 <= path(0);
--	c_p4 <= path(0);
--	d_p4 <= path(0);
--	e_p4 <= path(0);

--	non_sense:
--	process (in2, path)
--		variable counter : integer range 0 to 1;
--	begin
--		if (rising_edge(in2)) then
--			counter := counter + 1;
--			b_p1 <= path(counter);
--			c_p1 <= path(counter);
--			d_p1 <= path(counter);
--			e_p1 <= path(counter);
--			b_p2 <= path(counter);
--			c_p2 <= path(counter);
--			d_p2 <= path(counter);
--			e_p2 <= path(counter);
--			b_p3 <= path(counter);
--			c_p3 <= path(counter);
--			d_p3 <= path(counter);
--			e_p3 <= path(counter);
--			b_p4 <= path(counter);
--			c_p4 <= path(counter);
--			d_p4 <= path(counter);
--			e_p4 <= path(counter);
--		end if;
--	end process;
	
end architecture;

