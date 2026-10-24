// Test-only observers: no outputs and no control/prediction feedback.
// The generated header checks each signature present in the elaborated DUT.
// Verilator compiles this classpath resource as a user C++ source in its Mdir.
#include "VsvsimTestbench__Dpi.h"

extern "C" void h2p_hint_pending(unsigned int, unsigned int, unsigned int) {}
extern "C" void h2p_hint_profile_trace(unsigned int, unsigned long long,
    unsigned long long, unsigned int, unsigned int, unsigned long long,
    unsigned long long, svBit, svBit, svBit, svBit, svBit, svBit) {}
extern "C" void h2p_hint_query_v4(unsigned int, unsigned int,
    unsigned long long, unsigned long long, unsigned long long, unsigned int,
    svBit, svBit, svBit, svBit, unsigned int, unsigned int, unsigned int,
    unsigned int, unsigned int, unsigned int) {}

extern "C" void h2p_hint_prediction(unsigned int, unsigned long long,
    unsigned long long, unsigned long long, unsigned long long, unsigned long long,
    unsigned int, svBit, svBit, unsigned char,
    svBit, svBit, svBit, unsigned char, svBit, unsigned char,
    svBit, unsigned long long, svBit, unsigned long long, unsigned long long,
    svBit, svBit, svBit) {}

extern "C" void h2p_hint_trace_sink(svBit, svBit, svBit,
    unsigned long long, unsigned long long, unsigned long long,
    unsigned int, unsigned int, unsigned long long, unsigned int) {}

extern "C" void diagnostic_event(unsigned int, unsigned int,
    unsigned long long, // cycle
    unsigned long long, unsigned long long, unsigned long long, unsigned long long,
    unsigned long long, unsigned long long, unsigned long long, unsigned long long,
    unsigned long long, unsigned long long, unsigned long long, unsigned long long,
    unsigned long long, unsigned long long, unsigned long long, unsigned long long) {}
