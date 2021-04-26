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

#ifndef __LIBIEEE1666_DATA_TYPES_FIXED_POINT_FXNUM_BITREF_H__
#define __LIBIEEE1666_DATA_TYPES_FIXED_POINT_FXNUM_BITREF_H__

#include <data_types/fwd.h>
#include <iostream>

namespace sc_dt {

//////////////////////////////// declaration //////////////////////////////////

class sc_fxnum_bitref
{
	friend class sc_fxnum;
	friend class sc_fxnum_fast_bitref;

public:
	// Copy constructor
	sc_fxnum_bitref(const sc_fxnum_bitref&);

	// Assignment operators
#define DECL_ASN_OP_T(op, tp) \
	sc_fxnum_bitref& operator op (tp);

#define DECL_ASN_OP(op) \
	DECL_ASN_OP_T(op, const sc_fxnum_bitref&) \
	DECL_ASN_OP_T(op, const sc_fxnum_fast_bitref&) \
	DECL_ASN_OP_T(op, bool)

	DECL_ASN_OP(=)
	DECL_ASN_OP(&=)
	DECL_ASN_OP(|=)
	DECL_ASN_OP(^=)

#undef DECL_ASN_OP_T
#undef DECL_ASN_OP

	// Implicit conversion
	operator bool() const;

	// Print or dump content
	void print(std::ostream& = std::cout) const;
	void scan(std::istream& = std::cin);
	void dump(std::ostream& = std::cout) const;

private:
	// Disabled
	// Constructors
	sc_fxnum_bitref(sc_fxnum&, int);
	sc_fxnum_bitref();
};

} // end of namespace sc_dt

#endif
