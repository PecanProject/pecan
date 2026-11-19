"""Strict linear FP64 resampling and a gather-only FP32 matrix kernel."""

import time
import numpy as np

KERNEL = r"""
extern "C" __global__ void pack(
 float* out,const int* reps,int n,int cols,int steps,
 const unsigned long long* fields,const float* fallback,
 const float* coords,const float* statics,const int* static_year,
 const float* vi,const int* matched,int composites,
 const float* calendar,const int* zone,int zones,
 const float* soil,const int* rr,const int* cc,const unsigned char* inside,int nr,int nc,
 const float* numeric,const int* kg,const int* lc,const int* annual,const int* year) {
 const long long i=(long long)blockIdx.x*blockDim.x+threadIdx.x;
 if(i>=(long long)n*cols)return;
 const int col=i/n,p=i%n,k=reps[col];union {unsigned int u;float f;} nan_value;nan_value.u=0x7fc00000u;float value=nan_value.f;
 if(k<3968){const int t=k/31,v=k%31;
  if(t<steps){
   int f=-1;
   if(v<=4)f=v;else if(v==18)f=5;else if(v==20)f=6;else if(v==21)f=7;
   if(f>=0)value=(float)((const double*)fields[f])[(long long)t*n+p];
   else if(v==5)value=coords[p];else if(v==13)value=coords[n+p];
   else if(v==6||v==7||v==24){int a=v==6?0:(v==7?1:2);value=calendar[((long long)a*steps+t)*zones+zone[p]];}
   else if(v==8||v==9||v==22){int a=v==8?0:(v==9?1:2);value=vi[((long long)a*composites+matched[t])*n+p];}
   else if(v==19||v==23||v==29){int a=v==29?0:(v==23?1:2);value=fallback[((long long)t*3+a)*n+p];}
   else if(v>=25&&v<=28){if(inside[p])value=soil[(((long long)t*nr+rr[p])*nc+cc[p])*4+v-25];}
   else if(v==30){double sw=((const double*)fields[8])[(long long)t*n+p];union {double d;unsigned long long u;} bits;bits.d=sw;if((bits.u&0x7ff0000000000000ULL)!=0x7ff0000000000000ULL)value=sw>10.0?1.0f:0.0f;}
   else{int a=-1;if(v==10)a=0;else if(v==11)a=1;else if(v==12)a=2;else if(v==14)a=3;else if(v==15)a=4;else if(v==16)a=5;else if(v==17)a=6;if(a>=0)value=statics[((long long)static_year[t]*7+a)*n+p];}
  }
 }else if(k<4096)value=(k-3968<steps)?1.0f:0.0f;
 else if(k<4163)value=numeric[(long long)p*67+k-4096];
 else if(k<4186)value=kg[p]==k-4163?1.0f:0.0f;
 else if(k<4194)value=lc[p]==k-4186?1.0f:0.0f;
 else if(k<4206)value=annual[p]==k-4194?1.0f:0.0f;
 else if(k<4219)value=year[p]-2012==k-4206?1.0f:0.0f;
 else value=(float)steps/128.0f;
 out[i]=value;
}
"""


def build(runtime, payload):
    cp = runtime.cp
    data = payload["_gpu_native"]
    n = payload["meta"]["eligible_points"]
    t = data["steps"]
    times = {}
    tick = time.perf_counter()
    gweather = {}
    for item in data["weather"]["fields"]:
        name = item["name"]
        r0, r1, wy, iy, c0, c1, wx, ix = data["weather"]["geometry"][item["year"]]
        z = cp.asarray(item["z"])
        value = cp.zeros((len(item["chosen"]), n), cp.float64)
        allfinite = np.isfinite(item["z"]).all()
        total = cp.zeros(n if allfinite else value.shape, cp.float64)
        for rr, cc, w in (
            (r0, c0, (1 - wy) * (1 - wx)),
            (r0, c1, (1 - wy) * wx),
            (r1, c0, wy * (1 - wx)),
            (r1, c1, wy * wx),
        ):
            a = z[:, cp.asarray(rr - item["rlo"]), cp.asarray(cc - item["clo"])]
            weight = cp.asarray(w)
            if allfinite:
                weight = cp.where(weight > 1e-12, weight, 0)
                value += a * weight
                total += weight
            else:
                good = cp.isfinite(a) & (weight[None, :] > 1e-12)
                value += cp.where(good, a * weight, 0)
                total += cp.where(good, weight, 0)
        if allfinite:
            value /= total[None, :]
            value[:, total <= 0] = cp.nan
        else:
            value = value / cp.where(total > 0, total, 1)
            value[total <= 0] = cp.nan
        value[:, cp.asarray(~(ix & iy))] = cp.nan
        if name not in gweather:
            gweather[name] = cp.full((t, n), cp.nan, cp.float64)
        gweather[name][cp.asarray(item["chosen"])] = value
    ppfd = gweather["ssrd"] / 10800.0 * 0.45 * 4.57
    temp2 = gweather["t2m"] * gweather["t2m"]
    ppfd2 = ppfd * ppfd
    field_arrays = [gweather[k] for k in ("t2m", "sp", "d2m", "tp", "strd")] + [
        ppfd,
        temp2,
        ppfd2,
        gweather["ssrd"],
    ]
    pointers = cp.asarray(np.array([a.data.ptr for a in field_arrays], np.uint64))
    times["gpu_linear_sources"] = time.perf_counter() - tick
    tick = time.perf_counter()
    fallback = cp.asarray(payload["x"][:, : data["fallback_columns"]])
    coords = cp.asarray(data["coords"])
    years = sorted(data["static"])
    statics = cp.asarray(np.stack([data["static"][y] for y in years]))
    static_year = cp.asarray(
        np.array([years.index(int(y)) for y in data["step_year"]], np.int32)
    )
    vi = cp.asarray(data["vi"])
    matched = cp.asarray(data["vi_matched"])
    calendar = cp.asarray(data["calendar"])
    zone = cp.asarray(data["zone_codes"])
    soil = cp.asarray(data["soil"])
    rr = cp.asarray(data["soil_rows"])
    cc = cp.asarray(data["soil_cols"])
    inside = cp.asarray(data["soil_inside"])
    numeric = cp.asarray(payload["numeric"])
    kg = cp.asarray(data["kg"])
    lc = cp.asarray(data["lc"])
    annual = cp.asarray(payload["annual"], dtype=cp.int32)
    year = cp.asarray(payload["year"], dtype=cp.int32)
    reps = cp.asarray(runtime.reps, dtype=cp.int32)
    required = n * len(runtime.reps) * 4
    if required > runtime.capacity:
        runtime.buffer = cp.cuda.alloc(required)
        runtime.capacity = required
    out = cp.ndarray(
        (n, len(runtime.reps)), cp.float32, memptr=runtime.buffer, order="F"
    )
    if not hasattr(runtime, "native_kernel"):
        runtime.native_kernel = cp.RawKernel(KERNEL, "pack", options=("--fmad=false",))
    runtime.native_kernel(
        ((n * len(runtime.reps) + 255) // 256,),
        (256,),
        (
            out,
            reps,
            np.int32(n),
            np.int32(len(runtime.reps)),
            np.int32(t),
            pointers,
            fallback,
            coords,
            statics,
            static_year,
            vi,
            matched,
            np.int32(data["vi"].shape[1]),
            calendar,
            zone,
            np.int32(data["calendar"].shape[2]),
            soil,
            rr,
            cc,
            inside,
            np.int32(data["soil"].shape[1]),
            np.int32(data["soil"].shape[2]),
            numeric,
            kg,
            lc,
            annual,
            year,
        ),
    )
    cp.cuda.get_current_stream().synchronize()
    times["gpu_upload_pack"] = time.perf_counter() - tick
    return out, times
