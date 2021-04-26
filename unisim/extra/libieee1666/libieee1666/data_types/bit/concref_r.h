/*
 *  Copyright (c) 2017,
 *  Commissariat a l'Energie Atomique (CEA)
 *  All rights reserved.
 *
 *  Redistribution and use in source and binary forms, with or without modification,
 *  are permitted provided that the following conditions are met:
 *
 *   - Redistributions of source code must retain the above copyright notice, this
 *     list of conditions and the following disclaimer.
 *
 *   - Redistributions in binary form must reproduce the above copyright notice,
 *     this list of conditions and the following disclaimer in the documentation
 *     and/or other materials provided with the distribution.
 *
 *   - Neither the name of CEA nor the names of its contributors may be used to
 *     endorse or promote products derived from this software without specific prior
 *     written permission.
 *
 *  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 *  ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 *  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 *  DISCLAIMED.
 *  IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
 *  INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
 *  BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 *  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY
 *  OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 *  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,
 *  EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * Authors: Gilles Mouchard (gilles.mouchard@cea.fr)
 */

#ifndef __LIBIEEE1666_DATA_TYPES_BIT_CONCREF_R_H__
#define __LIBIEEE1666_DATA_TYPES_BIT_CONCREF_R_H__

#include <data_types/fwd.h>
#include <iostream>

namespace sc_dt {

//////////////////////////////// declaration //////////////////////////////////

template <class T1, class T2>
class sc_concref_r 
{
public:
	// Copy constructor
	sc_concref_r(const sc_concref_r<T1,T2>& a);

	// Destructor
	virtual ~sc_concref_r();

	// Bit selection
	sc_bitref_r<sc_concref_r<T1,T2> > operator [] (int i) const;

	// Part selection
	sc_subref_r<sc_concref_r<T1,T2> > operator () (int hi, int lo) const;
	sc_subref_r<sc_concref_r<T1,T2> > range(int hi, int lo) const;

	// Reduce functions
	sc_logic_value_t and_reduce() const;
	sc_logic_value_t nand_reduce() const;
	sc_logic_value_t or_reduce() const;
	sc_logic_value_t nor_reduce() const;
	sc_logic_value_t xor_reduce() const;
	sc_logic_value_t xnor_reduce() const;

	// Common methods
	int length() const;

	// Explicit conversions to character string
	const std::string to_string() const;
	const std::string to_string(sc_numrep) const;
	const std::string to_string(sc_numrep, bool) const;

	// Explicit conversions
	int to_int() const;
	unsigned int to_uint() const;
	long to_long() const;
	unsigned long to_ulong() const;
	int64 to_int64() const;
	uint64 to_uint64() const;
	bool is_01() const;

	// Other methods
	void print(std::ostream& os = std::cout) const;

private:
	// Disabled
	sc_concref_r();
	sc_concref_r<T1,T2>& operator = (const sc_concref_r<T1,T2>&);
};

///////////////////////////////// definition //////////////////////////////////

// Copy constructor
template <class T1, class T2>
sc_concref_r<T1,T2>::sc_concref_r(const sc_concref_r<T1,T2>& a)
{
}

// Destructor
template <class T1, class T2>
sc_concref_r<T1,T2>::~sc_concref_r()
{
}

// Bit selection
template <class T1, class T2>
sc_bitref_r<sc_concref_r<T1,T2> > sc_concref_r<T1,T2>::operator [] (int i) const
{
}

// Part selection
template <class T1, class T2>
sc_subref_r<sc_concref_r<T1,T2> > sc_concref_r<T1,T2>::operator () (int hi, int lo) const
{
}

template <class T1, class T2>
sc_subref_r<sc_concref_r<T1,T2> > sc_concref_r<T1,T2>::range(int hi, int lo) const
{
}

// Reduce functions
template <class T1, class T2>
sc_logic_value_t sc_concref_r<T1,T2>::and_reduce() const
{
}

template <class T1, class T2>
sc_logic_value_t sc_concref_r<T1,T2>::nand_reduce() const
{
}

template <class T1, class T2>
sc_logic_value_t sc_concref_r<T1,T2>::or_reduce() const
{
}

template <class T1, class T2>
sc_logic_value_t sc_concref_r<T1,T2>::nor_reduce() const
{
}

template <class T1, class T2>
sc_logic_value_t sc_concref_r<T1,T2>::xor_reduce() const
{
}

template <class T1, class T2>
sc_logic_value_t sc_concref_r<T1,T2>::xnor_reduce() const
{
}

// Common methods
template <class T1, class T2>
int sc_concref_r<T1,T2>::length() const
{
}

// Explicit conversions to character string
template <class T1, class T2>
const std::string sc_concref_r<T1,T2>::to_string() const
{
}

template <class T1, class T2>
const std::string sc_concref_r<T1,T2>::to_string(sc_numrep) const
{
}

template <class T1, class T2>
const std::string sc_concref_r<T1,T2>::to_string(sc_numrep, bool) const
{
}

// Explicit conversions
template <class T1, class T2>
int sc_concref_r<T1,T2>::to_int() const
{
}

template <class T1, class T2>
unsigned int sc_concref_r<T1,T2>::to_uint() const
{
}

template <class T1, class T2>
long sc_concref_r<T1,T2>::to_long() const
{
}

template <class T1, class T2>
unsigned long sc_concref_r<T1,T2>::to_ulong() const
{
}

template <class T1, class T2>
int64 sc_concref_r<T1,T2>::to_int64() const
{
}

template <class T1, class T2>
uint64 sc_concref_r<T1,T2>::to_uint64() const
{
}

template <class T1, class T2>
bool sc_concref_r<T1,T2>::is_01() const
{
}

// Other methods
template <class T1, class T2>
void sc_concref_r<T1,T2>::print(std::ostream& os) const
{
}

// Disabled
template <class T1, class T2>
sc_concref_r<T1,T2>::sc_concref_r()
{
}

template <class T1, class T2>
sc_concref_r<T1,T2>& sc_concref_r<T1,T2>::operator = (const sc_concref_r<T1,T2>&)
{
}

} // end of namespace sc_dt

#endif
