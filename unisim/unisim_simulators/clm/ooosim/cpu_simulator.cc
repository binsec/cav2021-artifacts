/*
 *  Copyright (c) 2007,
 *  University of Perpignan (UPVD),
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
 * Authors: 
 *     David Parello (david.parello@univ-perp.fr)
 *
 */

/***************************************************************************
                            simulator.hh  -  description
 ***************************************************************************/


#include <unisim/component/clm/processor/ooosim/cpu_simulator.hh>
#include <unisim/component/clm/processor/ooosim/cpu_simulator.tcc>
#include <unisim/component/clm/processor/ooosim/parameters.hh>

#include <unisim/component/clm/interfaces/memreq.hh>
#include <unisim/component/clm/interfaces/memreq.tcc>


namespace unisim {
namespace component {
namespace clm {
namespace interfaces {

  template class memreq<unisim::component::clm::interfaces::InstructionPtr,
			unisim::component::clm::processor::ooosim::DL1_nCPUtoCacheDataPathSize>;
  template class memreq<unisim::component::clm::interfaces::InstructionPtr,
			unisim::component::clm::processor::ooosim::DL1_nCachetoMemDataPathSize>;

  template ostream & operator<<(ostream &os, const memreq<unisim::component::clm::interfaces::InstructionPtr,
				unisim::component::clm::processor::ooosim::DL1_nCachetoMemDataPathSize> &req);


} } } }

namespace unisim {
  namespace component {
    namespace clm {
      namespace processor {
	namespace ooosim {
	  
	  using unisim::component::clm::processor::ooosim::OooSimCpu;

	  template class OooSimCpu<nIntegerRegisters,IL1_nCachetoCPUDataPathSize,IL1_nCPUtoCacheDataPathSize,DL1_nCachetoCPUDataPathSize,DL1_nCPUtoCacheDataPathSize,nProg, false>;
	  
	} // end of namespace ooosim
      } // end of namespace processor
    } // end of namespace clm
  } // end of namespace component
} // end of namespace unisim


