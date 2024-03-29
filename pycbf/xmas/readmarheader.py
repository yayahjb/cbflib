#!/usr/bin/env python
import struct

# Convert mar c header file types to python struct module types
mar_c_to_python_struct = {
    "INT32"  : "i",
    "UINT32" : "I",
    "char"   : "c",
    "UINT16" : "H"
    }

# Sizes (bytes) of mar c header objects
mar_c_sizes = {
    "INT32"  : 4,
    "UINT32" : 4,
    "char"   : 1,
    "UINT16" : 2
    }

# This was worked out by trial and error from a trial image I think
MAXIMAGES=9



def make_format(cdefinition):
    """
    Reads the header definition in c and makes the format 
    string to pass to struct.unpack
    """
    lines = cdefinition.split("\n")
    fmt = ""
    names = []
    expected = 0
    for line in lines:
        if line.find(";")==-1:
            continue
        decl  = line.split(";")[0].lstrip().rstrip()
        try:
            [type, name] = decl.split()
        except:
            #print("skipping:",line)
            continue
        #        print("type:",type,"  name:",name)

        if name.find("[")>-1:
            # repeated ... times
            try:
                num = name.split("[")[1].split("]")[0]
                num = num.replace("MAXIMAGES",str(MAXIMAGES))
                num = num.replace("sizeof(INT32)","4")
                times = eval(num)
            except:
                print("Please decode",decl)
                raise
        else:
            times=1
        try:
            fmt   += mar_c_to_python_struct[type]*times
            names += [name]*times
            expected += mar_c_sizes[type]*times
        except:
            #print("skipping",line)
            continue
        #print("%4d %4d"%(mar_c_sizes[type]*times,expected),name,":",times,line)
    #print(struct.calcsize(fmt),expected)
    return names, fmt

def read_mar_header(filename):
    """
    Get the header from a binary file
    """
    f = open(filename,"rb")
    f.seek(1024)
    header=f.read(3072)
    f.close()
    return header


def interpret_header(header, fmt, names):
    """
    given a format and header interpret it
    """
    values = struct.unpack(fmt,header)
    dict = {}
    i=0
    for name in names:
        if name in dict:
            if type(values[i]) == type("string"): 
                 dict[name] = dict[name]+values[i]
            else:
                 try:
                     dict[name].append(values[i])
                 except:
                     dict[name] = [dict[name],values[i]]
        else:
            dict[name] = values[i]
        i=i+1

    return dict


# Now for the c definition (found on mar webpage)
# The following string is therefore copyrighted by Mar I guess
        
cdefinition = """
typedef struct frame_header_type {
         /* File/header format parameters (256 bytes) */
         UINT32        header_type;      /* flag for header type  
                                           (can be  used as magic number) */
         char header_name[16];           /* header name (MMX) */
         UINT32        header_major_version;     /* header_major_version  (n.) */
         UINT32        header_minor_version;     /* header_minor_version  (.n) */
         UINT32        header_byte_order;/* BIG_ENDIAN (Motorola,MIPS);  
                                            LITTLE_ENDIAN (DEC, Intel) */
         UINT32        data_byte_order;  /* BIG_ENDIAN (Motorola,MIPS);  
                                            LITTLE_ENDIAN (DEC, Intel) */
         UINT32        header_size;      /* in bytes                     */
         UINT32        frame_type;       /* flag for frame type */
         UINT32        magic_number;     /* to be used as a flag - 
                                            usually  to indicate new file */
         UINT32        compression_type; /* type of image compression    */
         UINT32        compression1;     /* compression parameter 1 */
         UINT32        compression2;     /* compression parameter 2 */
         UINT32        compression3;     /* compression parameter 3 */
         UINT32        compression4;     /* compression parameter 4 */
         UINT32        compression5;     /* compression parameter 4 */
         UINT32        compression6;     /* compression parameter 4 */
         UINT32        nheaders;         /* total number of headers      */
         UINT32        nfast;            /* number of pixels in one line */
         UINT32        nslow;            /* number of lines in image     */
         UINT32        depth;            /* number of bytes per pixel    */
         UINT32        record_length;    /* number of pixels between 
                                            succesive rows */
         UINT32        signif_bits;      /* true depth of data, in bits  */
         UINT32        data_type;        /* (signed,unsigned,float...) */
         UINT32        saturated_value;  /* value marks pixel as saturated */
         UINT32        sequence;         /* TRUE or FALSE */
         UINT32        nimages;          /* total number of images - size of 
                                            each is nfast*(nslow/nimages) */
         UINT32        origin;           /* corner of origin             */
         UINT32        orientation;      /* direction of fast axis       */
         UINT32        view_direction;   /* direction to view frame      */
         UINT32        overflow_location;/* FOLLOWING_HEADER,  FOLLOWING_DATA */
         UINT32        over_8_bits;      /* # of pixels with counts  255 */
         UINT32        over_16_bits;     /* # of pixels with count  65535 */
         UINT32        multiplexed;      /* multiplex flag */
         UINT32        nfastimages;      /* # of images in fast direction */
         UINT32        nslowimages;      /* # of images in slow direction */
         UINT32        background_applied; /* flags correction has been applied - 
                                              hold magic number ? */
         UINT32        bias_applied;       /* flags correction has been applied - 
                                              hold magic number ? */
         UINT32        flatfield_applied;  /* flags correction has been applied - 
                                              hold magic number ? */
         UINT32        distortion_applied; /* flags correction has been applied - 
                                              hold magic number ? */
         UINT32        original_header_type;     /* Header/frame type from  file 
                                                    that frame is read from */
         UINT32        file_saved;         /* Flag that file has been  saved, 
                                              should be zeroed if modified */
         char reserve1[(64-40)*sizeof(INT32)-16];

         /* Data statistics (128) */
         UINT32        total_counts[2];  /* 64 bit integer range = 1.85E19*/
         UINT32        special_counts1[2];
         UINT32        special_counts2[2];
         UINT32        min;
         UINT32        max;
         UINT32        mean;
         UINT32        rms;
         UINT32        p10;
         UINT32        p90;
         UINT32        stats_uptodate;
         UINT32        pixel_noise[MAXIMAGES]; /* 1000*base noise value (ADUs) */
         char reserve2[(32-13-MAXIMAGES)*sizeof(INT32)];

         /* More statistics (256) */
         UINT16 percentile[128];


         /* Goniostat parameters (128 bytes) */
         INT32 xtal_to_detector;  /* 1000*distance in millimeters */
         INT32 beam_x;            /* 1000*x beam position (pixels) */
         INT32 beam_y;            /* 1000*y beam position (pixels) */
         INT32 integration_time;  /* integration time in  milliseconds */
         INT32 exposure_time;     /* exposure time in milliseconds */
         INT32 readout_time;      /* readout time in milliseconds */
         INT32 nreads;            /* number of readouts to get this  image */
         INT32 start_twotheta;    /* 1000*two_theta angle */
         INT32 start_omega;       /* 1000*omega angle */
         INT32 start_chi;         /* 1000*chi angle */
         INT32 start_kappa;       /* 1000*kappa angle */
         INT32 start_phi;         /* 1000*phi angle */
         INT32 start_delta;       /* 1000*delta angle */
         INT32 start_gamma;       /* 1000*gamma angle */
         INT32 start_xtal_to_detector; /* 1000*distance in mm (dist in um)*/
         INT32 end_twotheta;           /* 1000*two_theta angle */
         INT32 end_omega;              /* 1000*omega angle */
         INT32 end_chi;                /* 1000*chi angle */
         INT32 end_kappa;              /* 1000*kappa angle */
         INT32 end_phi;                /* 1000*phi angle */
         INT32 end_delta;              /* 1000*delta angle */
         INT32 end_gamma;              /* 1000*gamma angle */
         INT32 end_xtal_to_detector;   /* 1000*distance in mm (dist in um)*/
         INT32 rotation_axis;          /* active rotation axis */
         INT32 rotation_range;         /* 1000*rotation angle */
         INT32 detector_rotx;          /* 1000*rotation of detector  around X */
         INT32 detector_roty;          /* 1000*rotation of detector  around Y */
         INT32 detector_rotz;          /* 1000*rotation of detector  around Z */
         char reserve3[(32-28)*sizeof(INT32)];

         /* Detector parameters (128 bytes) */
         INT32 detector_type;            /* detector type */
         INT32 pixelsize_x;              /* pixel size (nanometers) */
         INT32 pixelsize_y;              /* pixel size (nanometers) */
         INT32 mean_bias;                        /* 1000*mean bias value */
         INT32 photons_per_100adu;       /* photons / 100 ADUs */
         INT32 measured_bias[MAXIMAGES]; /* 1000*mean bias value for each image*/
         INT32 measured_temperature[MAXIMAGES];  /* Temperature of each  
                                                    detector in milliKelvins */
         INT32 measured_pressure[MAXIMAGES]; /* Pressure of each  chamber 
                                               in microTorr */
         /* Retired reserve4 when MAXIMAGES set to 9 from 16 and 
            two fields removed, and temp and pressure added
          char reserve4[(32-(5+3*MAXIMAGES))*sizeof(INT32)]
         */

         /* X-ray source and optics parameters (128 bytes) */
         /* X-ray source parameters (8*4 bytes) */
         INT32 source_type;              /* (code) - target, synch. etc */
         INT32 source_dx;                /* Optics param. - (size  microns) */
         INT32 source_dy;                /* Optics param. - (size  microns) */
         INT32 source_wavelength;        /* wavelength  (femtoMeters) */
         INT32 source_power;             /* (Watts) */
         INT32 source_voltage;           /* (Volts) */
         INT32 source_current;           /* (microAmps) */
         INT32 source_bias;              /* (Volts) */
         INT32 source_polarization_x;    /* () */
         INT32 source_polarization_y;    /* () */
         char reserve_source[4*sizeof(INT32)];

         /* X-ray optics_parameters (8*4 bytes) */
         INT32 optics_type;              /* Optics type (code)*/
         INT32 optics_dx;                /* Optics param. - (size  microns) */
         INT32 optics_dy;                /* Optics param. - (size  microns) */
         INT32 optics_wavelength;        /* Optics param. - (size  microns) */
         INT32 optics_dispersion;        /* Optics param. - (*10E6) */
         INT32 optics_crossfire_x;       /* Optics param. - (microRadians) */
         INT32 optics_crossfire_y;       /* Optics param. - (microRadians) */
         INT32 optics_angle;             /* Optics param. - (monoch.  
                                                    2theta - microradians) */
         INT32 optics_polarization_x;    /* () */
         INT32 optics_polarization_y;    /* () */
         char reserve_optics[4*sizeof(INT32)];

         char reserve5[((32-28)*sizeof(INT32))];

         /* File parameters (1024 bytes) */
         char filetitle[128];            /*  Title                  */
         char filepath[128];             /* path name for data  file  */
         char filename[64];              /* name of data  file  */
         char acquire_timestamp[32];     /* date and time of  acquisition */
         char header_timestamp[32];      /* date and time of header  update  */
         char save_timestamp[32];        /* date and time file  saved */
         char file_comments[512];        /* comments, use as desired   */
         char reserve6[1024-(128+128+64+(3*32)+512)];

         /* Dataset parameters (512 bytes) */
         char dataset_comments[512];     /* comments, used as desired   */
         /* pad out to  3072 bytes */
         char pad[3072-(256+128+256+(3*128)+1024+512)];     

         } frame_header;
"""



class marheaderreader:
    """
    Class to sit and read a series of images (makes format etc only once)
    """
    def __init__(self):
        """
        Initialise internal stuff
        """
        self.names , self.fmt = make_format(cdefinition)
    def get_header(self,filename):
        """
        Reads a header from file filename
        """
        h=read_mar_header(filename)
        dict = interpret_header(h,self.fmt,self.names)
        # Append ESRF formatted stuff
        items = self.readesrfstring(dict["dataset_comments[512]"])
        for pair in items:
            dict[pair[0]]=pair[1]
        items = self.readesrfstring(dict["file_comments[512]"])
        for pair in items:
            dict[pair[0]]=pair[1]
        dict["pixelsize_x_mm"]= str(float(dict["pixelsize_x"])/1e6)
        dict["pixelsize_y_mm"]= str(float(dict["pixelsize_y"])/1e6)
        dict["integration_time_sec"]= str(float(dict["integration_time"])/1e3)
        dict["beam_y_mm"]= str(float(dict["pixelsize_y_mm"])*
                                         float(dict["beam_y"])/1000.)
        dict["beam_x_mm"]= str(float(dict["pixelsize_x_mm"])*
                                         float(dict["beam_x"])/1000.)
        
        return dict
    
    def readesrfstring(self,s):
        """
        Interpret the so called "esrf format" header lines 
        which are in comment sections
        """
        s=s.replace("\000","")
        items = [_f for _f in [len(x)>1 and x or None for x in [
            item.split("=") for item in s.split(";")]] if _f]
        return items


if __name__=="__main__":
    """
    Make a little program to process files
    """
    import sys
    print("Starting")
    names,fmt = make_format(cdefinition)
    print("Names and format made")
    h = read_mar_header(sys.argv[1])
    print("Read header, interpreting")
    d = interpret_header(h,fmt,names)
    printed = {}
    for name in names:
        if name in printed:
            continue
        print(name,":",d[name])
        printed[name]=1

