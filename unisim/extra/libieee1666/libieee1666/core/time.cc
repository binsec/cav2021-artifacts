/*
 *  Copyright (c) 2014,
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

#include "core/time.h"
#include "core/kernel.h"
#include <sstream>

namespace sc_core {

const sc_time SC_ZERO_TIME;

//////////////////////////////////// sc_time /////////////////////////////////////

sc_time::sc_time(double d, sc_time_unit tu)
	: discrete_value(sc_kernel::get_kernel()->get_time_discrete_value(d, tu))
{
}

double sc_time::to_seconds() const
{
	return sc_kernel::get_kernel()->time_discrete_value_to_seconds(discrete_value);
}

const std::string sc_time::to_string() const
{
	std::stringstream sstr;
	sc_kernel::get_kernel()->print_time(sstr, *this);
	return sstr.str();
}

void sc_time::print(std::ostream& os) const
{
	sc_kernel::get_kernel()->print_time(os, *this);
}

//////////////////////////////// global functions /////////////////////////////////

std::ostream& operator << (std::ostream& os, const sc_time& t)
{
	t.print(os);
	return os;
}

void sc_set_time_resolution(double d, sc_time_unit tu)
{
	sc_kernel::get_kernel()->set_time_resolution(d, tu, true);
}

sc_time sc_get_time_resolution()
{
	return sc_kernel::get_kernel()->get_time_resolution();
}

const sc_time& sc_max_time()
{
	return sc_kernel::get_kernel()->get_max_time();
}

} // end of namespace sc_core
