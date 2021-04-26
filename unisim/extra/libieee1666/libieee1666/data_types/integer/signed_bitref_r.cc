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

#include <data_types/integer/signed_bitref_r.h>

namespace sc_dt {

///////////////////////////////// definition //////////////////////////////////

// Copy constructor
sc_signed_bitref_r::sc_signed_bitref_r(const sc_signed_bitref_r& a)
{
}

// Destructor
sc_signed_bitref_r::~sc_signed_bitref_r()
{
}

// Capacity
int sc_signed_bitref_r::length() const
{
}

// Implicit conversion to uint64
sc_signed_bitref_r::operator uint64 () const
{
}

bool sc_signed_bitref_r::operator ! () const
{
}

bool sc_signed_bitref_r::operator ~ () const
{
}

// Explicit conversions
bool sc_signed_bitref_r::to_bool() const
{
}

// Other methods
void sc_signed_bitref_r::print(std::ostream& os) const
{
}

sc_signed_bitref_r::sc_signed_bitref_r()
{
}

// Disabled
sc_signed_bitref_r& sc_signed_bitref_r::operator = (const sc_signed_bitref_r&)
{
}

} // end of namespace sc_dt
