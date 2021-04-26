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

#ifndef __LIBIEEE1666_DATA_TYPES_INTEGER_SIGNED_H__
#define __LIBIEEE1666_DATA_TYPES_INTEGER_SIGNED_H__

#include <data_types/fwd.h>
#include <data_types/context/length_param.h>
#include <data_types/integer/value_base.h>
#include <iostream>

namespace sc_dt {

//////////////////////////////// declaration //////////////////////////////////

class sc_signed : public sc_value_base
{
	friend class sc_concatref;
	friend class sc_signed_bitref_r;
	friend class sc_signed_bitref;
	friend class sc_signed_subref_r;
	friend class sc_signed_subref;
	friend class sc_unsigned;
	friend class sc_unsigned_subref;

public:
	// Constructors
	explicit sc_signed(int nb = sc_length_param().len());
	sc_signed(const sc_signed& v);
	sc_signed(const sc_unsigned& v);
	template <class T> explicit sc_signed(const sc_generic_base<T>& v);
	explicit sc_signed(const sc_bv_base& v);
	explicit sc_signed(const sc_lv_base& v);
	explicit sc_signed(const sc_int_subref_r& v);
	explicit sc_signed(const sc_uint_subref_r& v);
	explicit sc_signed(const sc_signed_subref_r& v);
	explicit sc_signed(const sc_unsigned_subref_r& v);

	// Assignment operators
	sc_signed& operator = (const sc_signed& v);
	sc_signed& operator = (const sc_signed_subref_r& a);
	template <class T> sc_signed& operator = (const sc_generic_base<T>& a);
	sc_signed& operator = (const sc_unsigned& v);
	sc_signed& operator = (const sc_unsigned_subref_r& a);
	sc_signed& operator = (const char *v);
	sc_signed& operator = (int64 v);
	sc_signed& operator = (uint64 v);
	sc_signed& operator = (long v);
	sc_signed& operator = (unsigned long v);
	sc_signed& operator = (int v);
	sc_signed& operator = (unsigned int v);
	sc_signed& operator = (double v);
	sc_signed& operator = (const sc_int_base& v);
	sc_signed& operator = (const sc_uint_base& v);
	sc_signed& operator = (const sc_bv_base&);
	sc_signed& operator = (const sc_lv_base&);
	sc_signed& operator = (const sc_fxval&);
	sc_signed& operator = (const sc_fxval_fast&);
	sc_signed& operator = (const sc_fxnum&);
	sc_signed& operator = (const sc_fxnum_fast&);

	// Destructor
	~sc_signed();

	// Increment operators.
	sc_signed& operator ++ ();
	const sc_signed operator ++ (int);

	// Decrement operators.
	sc_signed& operator -- ();
	const sc_signed operator -- (int);

	// Bit selection
	sc_signed_bitref operator[] (int i);
	sc_signed_bitref_r operator[] (int i) const;

	// Part selection
	sc_signed_subref range(int i, int j);
	sc_signed_subref_r range(int i, int j) const;
	sc_signed_subref operator() (int i, int j);
	sc_signed_subref_r operator() (int i, int j) const;

	// Explicit conversions
	int to_int() const;
	unsigned int to_uint() const;
	long to_long() const;
	unsigned long to_ulong() const;
	int64 to_int64() const;
	uint64 to_uint64() const;
	double to_double() const;

	// Explicit conversion to character string
	const std::string to_string(sc_numrep numrep = SC_DEC) const;
	const std::string to_string(sc_numrep numrep, bool w_prefix) const;

	// Print functions
	void print(std::ostream& os = std::cout) const;
	void scan(std::istream& is = std::cin);

	// Capacity
	int length() const;

	// Reduce methods
	bool and_reduce() const;
	bool nand_reduce() const;
	bool or_reduce() const;
	bool nor_reduce() const;
	bool xor_reduce() const;
	bool xnor_reduce() const;

	// Overloaded operators

};

///////////////////////////////// definition //////////////////////////////////

template <class T> sc_signed::sc_signed(const sc_generic_base<T>& v)
{
}

template <class T> sc_signed& sc_signed::operator = (const sc_generic_base<T>& a)
{
}

} // end of namespace sc_dt

#endif
