(in-package #:webgl-bindings)


(define-webgl-global (+DEPTH_BUFFER_BIT+ +DEPTH-BUFFER-BIT+) gl-enum)
(define-webgl-global (+STENCIL_BUFFER_BIT+ +STENCIL-BUFFER-BIT+) gl-enum)
(define-webgl-global (+COLOR_BUFFER_BIT+ +COLOR-BUFFER-BIT+) gl-enum)
(define-webgl-global (+POINTS+ +POINTS+) gl-enum)
(define-webgl-global (+LINES+ +LINES+) gl-enum)
(define-webgl-global (+LINE_LOOP+ +LINE-LOOP+) gl-enum)
(define-webgl-global (+LINE_STRIP+ +LINE-STRIP+) gl-enum)
(define-webgl-global (+TRIANGLES+ +TRIANGLES+) gl-enum)
(define-webgl-global (+TRIANGLE_STRIP+ +TRIANGLE-STRIP+) gl-enum)
(define-webgl-global (+TRIANGLE_FAN+ +TRIANGLE-FAN+) gl-enum)
(define-webgl-global (+ZERO+ +ZERO+) gl-enum)
(define-webgl-global (+ONE+ +ONE+) gl-enum)
(define-webgl-global (+SRC_COLOR+ +SRC-COLOR+) gl-enum)
(define-webgl-global (+ONE_MINUS_SRC_COLOR+ +ONE-MINUS-SRC-COLOR+) gl-enum)
(define-webgl-global (+SRC_ALPHA+ +SRC-ALPHA+) gl-enum)
(define-webgl-global (+ONE_MINUS_SRC_ALPHA+ +ONE-MINUS-SRC-ALPHA+) gl-enum)
(define-webgl-global (+DST_ALPHA+ +DST-ALPHA+) gl-enum)
(define-webgl-global (+ONE_MINUS_DST_ALPHA+ +ONE-MINUS-DST-ALPHA+) gl-enum)
(define-webgl-global (+DST_COLOR+ +DST-COLOR+) gl-enum)
(define-webgl-global (+ONE_MINUS_DST_COLOR+ +ONE-MINUS-DST-COLOR+) gl-enum)
(define-webgl-global (+SRC_ALPHA_SATURATE+ +SRC-ALPHA-SATURATE+) gl-enum)
(define-webgl-global (+FUNC_ADD+ +FUNC-ADD+) gl-enum)
(define-webgl-global (+BLEND_EQUATION+ +BLEND-EQUATION+) gl-enum)
(define-webgl-global (+BLEND_EQUATION_RGB+ +BLEND-EQUATION-RGB+) gl-enum)
(define-webgl-global (+BLEND_EQUATION_ALPHA+ +BLEND-EQUATION-ALPHA+) gl-enum)
(define-webgl-global (+FUNC_SUBTRACT+ +FUNC-SUBTRACT+) gl-enum)
(define-webgl-global (+FUNC_REVERSE_SUBTRACT+ +FUNC-REVERSE-SUBTRACT+) gl-enum)
(define-webgl-global (+BLEND_DST_RGB+ +BLEND-DST-RGB+) gl-enum)
(define-webgl-global (+BLEND_SRC_RGB+ +BLEND-SRC-RGB+) gl-enum)
(define-webgl-global (+BLEND_DST_ALPHA+ +BLEND-DST-ALPHA+) gl-enum)
(define-webgl-global (+BLEND_SRC_ALPHA+ +BLEND-SRC-ALPHA+) gl-enum)
(define-webgl-global (+CONSTANT_COLOR+ +CONSTANT-COLOR+) gl-enum)
(define-webgl-global (+ONE_MINUS_CONSTANT_COLOR+ +ONE-MINUS-CONSTANT-COLOR+) gl-enum)
(define-webgl-global (+CONSTANT_ALPHA+ +CONSTANT-ALPHA+) gl-enum)
(define-webgl-global (+ONE_MINUS_CONSTANT_ALPHA+ +ONE-MINUS-CONSTANT-ALPHA+) gl-enum)
(define-webgl-global (+BLEND_COLOR+ +BLEND-COLOR+) gl-enum)
(define-webgl-global (+ARRAY_BUFFER+ +ARRAY-BUFFER+) gl-enum)
(define-webgl-global (+ELEMENT_ARRAY_BUFFER+ +ELEMENT-ARRAY-BUFFER+) gl-enum)
(define-webgl-global (+ARRAY_BUFFER_BINDING+ +ARRAY-BUFFER-BINDING+) gl-enum)
(define-webgl-global (+ELEMENT_ARRAY_BUFFER_BINDING+ +ELEMENT-ARRAY-BUFFER-BINDING+) gl-enum)
(define-webgl-global (+STREAM_DRAW+ +STREAM-DRAW+) gl-enum)
(define-webgl-global (+STATIC_DRAW+ +STATIC-DRAW+) gl-enum)
(define-webgl-global (+DYNAMIC_DRAW+ +DYNAMIC-DRAW+) gl-enum)
(define-webgl-global (+BUFFER_SIZE+ +BUFFER-SIZE+) gl-enum)
(define-webgl-global (+BUFFER_USAGE+ +BUFFER-USAGE+) gl-enum)
(define-webgl-global (+CURRENT_VERTEX_ATTRIB+ +CURRENT-VERTEX-ATTRIB+) gl-enum)
(define-webgl-global (+FRONT+ +FRONT+) gl-enum)
(define-webgl-global (+BACK+ +BACK+) gl-enum)
(define-webgl-global (+FRONT_AND_BACK+ +FRONT-AND-BACK+) gl-enum)
(define-webgl-global (+CULL_FACE+ +CULL-FACE+) gl-enum)
(define-webgl-global (+BLEND+ +BLEND+) gl-enum)
(define-webgl-global (+DITHER+ +DITHER+) gl-enum)
(define-webgl-global (+STENCIL_TEST+ +STENCIL-TEST+) gl-enum)
(define-webgl-global (+DEPTH_TEST+ +DEPTH-TEST+) gl-enum)
(define-webgl-global (+SCISSOR_TEST+ +SCISSOR-TEST+) gl-enum)
(define-webgl-global (+POLYGON_OFFSET_FILL+ +POLYGON-OFFSET-FILL+) gl-enum)
(define-webgl-global (+SAMPLE_ALPHA_TO_COVERAGE+ +SAMPLE-ALPHA-TO-COVERAGE+) gl-enum)
(define-webgl-global (+SAMPLE_COVERAGE+ +SAMPLE-COVERAGE+) gl-enum)
(define-webgl-global (+NO_ERROR+ +NO-ERROR+) gl-enum)
(define-webgl-global (+INVALID_ENUM+ +INVALID-ENUM+) gl-enum)
(define-webgl-global (+INVALID_VALUE+ +INVALID-VALUE+) gl-enum)
(define-webgl-global (+INVALID_OPERATION+ +INVALID-OPERATION+) gl-enum)
(define-webgl-global (+OUT_OF_MEMORY+ +OUT-OF-MEMORY+) gl-enum)
(define-webgl-global (+CW+ +CW+) gl-enum)
(define-webgl-global (+CCW+ +CCW+) gl-enum)
(define-webgl-global (+LINE_WIDTH+ +LINE-WIDTH+) gl-enum)
(define-webgl-global (+ALIASED_POINT_SIZE_RANGE+ +ALIASED-POINT-SIZE-RANGE+) gl-enum)
(define-webgl-global (+ALIASED_LINE_WIDTH_RANGE+ +ALIASED-LINE-WIDTH-RANGE+) gl-enum)
(define-webgl-global (+CULL_FACE_MODE+ +CULL-FACE-MODE+) gl-enum)
(define-webgl-global (+FRONT_FACE+ +FRONT-FACE+) gl-enum)
(define-webgl-global (+DEPTH_RANGE+ +DEPTH-RANGE+) gl-enum)
(define-webgl-global (+DEPTH_WRITEMASK+ +DEPTH-WRITEMASK+) gl-enum)
(define-webgl-global (+DEPTH_CLEAR_VALUE+ +DEPTH-CLEAR-VALUE+) gl-enum)
(define-webgl-global (+DEPTH_FUNC+ +DEPTH-FUNC+) gl-enum)
(define-webgl-global (+STENCIL_CLEAR_VALUE+ +STENCIL-CLEAR-VALUE+) gl-enum)
(define-webgl-global (+STENCIL_FUNC+ +STENCIL-FUNC+) gl-enum)
(define-webgl-global (+STENCIL_FAIL+ +STENCIL-FAIL+) gl-enum)
(define-webgl-global (+STENCIL_PASS_DEPTH_FAIL+ +STENCIL-PASS-DEPTH-FAIL+) gl-enum)
(define-webgl-global (+STENCIL_PASS_DEPTH_PASS+ +STENCIL-PASS-DEPTH-PASS+) gl-enum)
(define-webgl-global (+STENCIL_REF+ +STENCIL-REF+) gl-enum)
(define-webgl-global (+STENCIL_VALUE_MASK+ +STENCIL-VALUE-MASK+) gl-enum)
(define-webgl-global (+STENCIL_WRITEMASK+ +STENCIL-WRITEMASK+) gl-enum)
(define-webgl-global (+STENCIL_BACK_FUNC+ +STENCIL-BACK-FUNC+) gl-enum)
(define-webgl-global (+STENCIL_BACK_FAIL+ +STENCIL-BACK-FAIL+) gl-enum)
(define-webgl-global (+STENCIL_BACK_PASS_DEPTH_FAIL+ +STENCIL-BACK-PASS-DEPTH-FAIL+) gl-enum)
(define-webgl-global (+STENCIL_BACK_PASS_DEPTH_PASS+ +STENCIL-BACK-PASS-DEPTH-PASS+) gl-enum)
(define-webgl-global (+STENCIL_BACK_REF+ +STENCIL-BACK-REF+) gl-enum)
(define-webgl-global (+STENCIL_BACK_VALUE_MASK+ +STENCIL-BACK-VALUE-MASK+) gl-enum)
(define-webgl-global (+STENCIL_BACK_WRITEMASK+ +STENCIL-BACK-WRITEMASK+) gl-enum)
(define-webgl-global (+VIEWPORT+ +VIEWPORT+) gl-enum)
(define-webgl-global (+SCISSOR_BOX+ +SCISSOR-BOX+) gl-enum)
(define-webgl-global (+COLOR_CLEAR_VALUE+ +COLOR-CLEAR-VALUE+) gl-enum)
(define-webgl-global (+COLOR_WRITEMASK+ +COLOR-WRITEMASK+) gl-enum)
(define-webgl-global (+UNPACK_ALIGNMENT+ +UNPACK-ALIGNMENT+) gl-enum)
(define-webgl-global (+PACK_ALIGNMENT+ +PACK-ALIGNMENT+) gl-enum)
(define-webgl-global (+MAX_TEXTURE_SIZE+ +MAX-TEXTURE-SIZE+) gl-enum)
(define-webgl-global (+MAX_VIEWPORT_DIMS+ +MAX-VIEWPORT-DIMS+) gl-enum)
(define-webgl-global (+SUBPIXEL_BITS+ +SUBPIXEL-BITS+) gl-enum)
(define-webgl-global (+RED_BITS+ +RED-BITS+) gl-enum)
(define-webgl-global (+GREEN_BITS+ +GREEN-BITS+) gl-enum)
(define-webgl-global (+BLUE_BITS+ +BLUE-BITS+) gl-enum)
(define-webgl-global (+ALPHA_BITS+ +ALPHA-BITS+) gl-enum)
(define-webgl-global (+DEPTH_BITS+ +DEPTH-BITS+) gl-enum)
(define-webgl-global (+STENCIL_BITS+ +STENCIL-BITS+) gl-enum)
(define-webgl-global (+POLYGON_OFFSET_UNITS+ +POLYGON-OFFSET-UNITS+) gl-enum)
(define-webgl-global (+POLYGON_OFFSET_FACTOR+ +POLYGON-OFFSET-FACTOR+) gl-enum)
(define-webgl-global (+TEXTURE_BINDING_2D+ +TEXTURE-BINDING-2D+) gl-enum)
(define-webgl-global (+SAMPLE_BUFFERS+ +SAMPLE-BUFFERS+) gl-enum)
(define-webgl-global (+SAMPLES+ +SAMPLES+) gl-enum)
(define-webgl-global (+SAMPLE_COVERAGE_VALUE+ +SAMPLE-COVERAGE-VALUE+) gl-enum)
(define-webgl-global (+SAMPLE_COVERAGE_INVERT+ +SAMPLE-COVERAGE-INVERT+) gl-enum)
(define-webgl-global (+NUM_COMPRESSED_TEXTURE_FORMATS+ +NUM-COMPRESSED-TEXTURE-FORMATS+) gl-enum)
(define-webgl-global (+COMPRESSED_TEXTURE_FORMATS+ +COMPRESSED-TEXTURE-FORMATS+) gl-enum)
(define-webgl-global (+DONT_CARE+ +DONT-CARE+) gl-enum)
(define-webgl-global (+FASTEST+ +FASTEST+) gl-enum)
(define-webgl-global (+NICEST+ +NICEST+) gl-enum)
(define-webgl-global (+GENERATE_MIPMAP_HINT+ +GENERATE-MIPMAP-HINT+) gl-enum)
(define-webgl-global (+BYTE+ +BYTE+) gl-enum)
(define-webgl-global (+UNSIGNED_BYTE+ +UNSIGNED-BYTE+) gl-enum)
(define-webgl-global (+SHORT+ +SHORT+) gl-enum)
(define-webgl-global (+UNSIGNED_SHORT+ +UNSIGNED-SHORT+) gl-enum)
(define-webgl-global (+INT+ +INT+) gl-enum)
(define-webgl-global (+UNSIGNED_INT+ +UNSIGNED-INT+) gl-enum)
(define-webgl-global (+FLOAT+ +FLOAT+) gl-enum)
(define-webgl-global (+DEPTH_COMPONENT+ +DEPTH-COMPONENT+) gl-enum)
(define-webgl-global (+ALPHA+ +ALPHA+) gl-enum)
(define-webgl-global (+RGB+ +RGB+) gl-enum)
(define-webgl-global (+RGBA+ +RGBA+) gl-enum)
(define-webgl-global (+LUMINANCE+ +LUMINANCE+) gl-enum)
(define-webgl-global (+LUMINANCE_ALPHA+ +LUMINANCE-ALPHA+) gl-enum)
(define-webgl-global (+UNSIGNED_SHORT_4_4_4_4+ +UNSIGNED-SHORT-4-4-4-4+) gl-enum)
(define-webgl-global (+UNSIGNED_SHORT_5_5_5_1+ +UNSIGNED-SHORT-5-5-5-1+) gl-enum)
(define-webgl-global (+UNSIGNED_SHORT_5_6_5+ +UNSIGNED-SHORT-5-6-5+) gl-enum)
(define-webgl-global (+FRAGMENT_SHADER+ +FRAGMENT-SHADER+) gl-enum)
(define-webgl-global (+VERTEX_SHADER+ +VERTEX-SHADER+) gl-enum)
(define-webgl-global (+MAX_VERTEX_ATTRIBS+ +MAX-VERTEX-ATTRIBS+) gl-enum)
(define-webgl-global (+MAX_VERTEX_UNIFORM_VECTORS+ +MAX-VERTEX-UNIFORM-VECTORS+) gl-enum)
(define-webgl-global (+MAX_VARYING_VECTORS+ +MAX-VARYING-VECTORS+) gl-enum)
(define-webgl-global (+MAX_COMBINED_TEXTURE_IMAGE_UNITS+ +MAX-COMBINED-TEXTURE-IMAGE-UNITS+) gl-enum)
(define-webgl-global (+MAX_VERTEX_TEXTURE_IMAGE_UNITS+ +MAX-VERTEX-TEXTURE-IMAGE-UNITS+) gl-enum)
(define-webgl-global (+MAX_TEXTURE_IMAGE_UNITS+ +MAX-TEXTURE-IMAGE-UNITS+) gl-enum)
(define-webgl-global (+MAX_FRAGMENT_UNIFORM_VECTORS+ +MAX-FRAGMENT-UNIFORM-VECTORS+) gl-enum)
(define-webgl-global (+SHADER_TYPE+ +SHADER-TYPE+) gl-enum)
(define-webgl-global (+DELETE_STATUS+ +DELETE-STATUS+) gl-enum)
(define-webgl-global (+LINK_STATUS+ +LINK-STATUS+) gl-enum)
(define-webgl-global (+VALIDATE_STATUS+ +VALIDATE-STATUS+) gl-enum)
(define-webgl-global (+ATTACHED_SHADERS+ +ATTACHED-SHADERS+) gl-enum)
(define-webgl-global (+ACTIVE_UNIFORMS+ +ACTIVE-UNIFORMS+) gl-enum)
(define-webgl-global (+ACTIVE_ATTRIBUTES+ +ACTIVE-ATTRIBUTES+) gl-enum)
(define-webgl-global (+SHADING_LANGUAGE_VERSION+ +SHADING-LANGUAGE-VERSION+) gl-enum)
(define-webgl-global (+CURRENT_PROGRAM+ +CURRENT-PROGRAM+) gl-enum)
(define-webgl-global (+NEVER+ +NEVER+) gl-enum)
(define-webgl-global (+LESS+ +LESS+) gl-enum)
(define-webgl-global (+EQUAL+ +EQUAL+) gl-enum)
(define-webgl-global (+LEQUAL+ +LEQUAL+) gl-enum)
(define-webgl-global (+GREATER+ +GREATER+) gl-enum)
(define-webgl-global (+NOTEQUAL+ +NOTEQUAL+) gl-enum)
(define-webgl-global (+GEQUAL+ +GEQUAL+) gl-enum)
(define-webgl-global (+ALWAYS+ +ALWAYS+) gl-enum)
(define-webgl-global (+KEEP+ +KEEP+) gl-enum)
(define-webgl-global (+REPLACE+ +REPLACE+) gl-enum)
(define-webgl-global (+INCR+ +INCR+) gl-enum)
(define-webgl-global (+DECR+ +DECR+) gl-enum)
(define-webgl-global (+INVERT+ +INVERT+) gl-enum)
(define-webgl-global (+INCR_WRAP+ +INCR-WRAP+) gl-enum)
(define-webgl-global (+DECR_WRAP+ +DECR-WRAP+) gl-enum)
(define-webgl-global (+VENDOR+ +VENDOR+) gl-enum)
(define-webgl-global (+RENDERER+ +RENDERER+) gl-enum)
(define-webgl-global (+VERSION+ +VERSION+) gl-enum)
(define-webgl-global (+NEAREST+ +NEAREST+) gl-enum)
(define-webgl-global (+LINEAR+ +LINEAR+) gl-enum)
(define-webgl-global (+NEAREST_MIPMAP_NEAREST+ +NEAREST-MIPMAP-NEAREST+) gl-enum)
(define-webgl-global (+LINEAR_MIPMAP_NEAREST+ +LINEAR-MIPMAP-NEAREST+) gl-enum)
(define-webgl-global (+NEAREST_MIPMAP_LINEAR+ +NEAREST-MIPMAP-LINEAR+) gl-enum)
(define-webgl-global (+LINEAR_MIPMAP_LINEAR+ +LINEAR-MIPMAP-LINEAR+) gl-enum)
(define-webgl-global (+TEXTURE_MAG_FILTER+ +TEXTURE-MAG-FILTER+) gl-enum)
(define-webgl-global (+TEXTURE_MIN_FILTER+ +TEXTURE-MIN-FILTER+) gl-enum)
(define-webgl-global (+TEXTURE_WRAP_S+ +TEXTURE-WRAP-S+) gl-enum)
(define-webgl-global (+TEXTURE_WRAP_T+ +TEXTURE-WRAP-T+) gl-enum)
(define-webgl-global (+TEXTURE_2D+ +TEXTURE-2D+) gl-enum)
(define-webgl-global (+TEXTURE+ +TEXTURE+) gl-enum)
(define-webgl-global (+TEXTURE_CUBE_MAP+ +TEXTURE-CUBE-MAP+) gl-enum)
(define-webgl-global (+TEXTURE_BINDING_CUBE_MAP+ +TEXTURE-BINDING-CUBE-MAP+) gl-enum)
(define-webgl-global (+TEXTURE_CUBE_MAP_POSITIVE_X+ +TEXTURE-CUBE-MAP-POSITIVE-X+) gl-enum)
(define-webgl-global (+TEXTURE_CUBE_MAP_NEGATIVE_X+ +TEXTURE-CUBE-MAP-NEGATIVE-X+) gl-enum)
(define-webgl-global (+TEXTURE_CUBE_MAP_POSITIVE_Y+ +TEXTURE-CUBE-MAP-POSITIVE-Y+) gl-enum)
(define-webgl-global (+TEXTURE_CUBE_MAP_NEGATIVE_Y+ +TEXTURE-CUBE-MAP-NEGATIVE-Y+) gl-enum)
(define-webgl-global (+TEXTURE_CUBE_MAP_POSITIVE_Z+ +TEXTURE-CUBE-MAP-POSITIVE-Z+) gl-enum)
(define-webgl-global (+TEXTURE_CUBE_MAP_NEGATIVE_Z+ +TEXTURE-CUBE-MAP-NEGATIVE-Z+) gl-enum)
(define-webgl-global (+MAX_CUBE_MAP_TEXTURE_SIZE+ +MAX-CUBE-MAP-TEXTURE-SIZE+) gl-enum)
(define-webgl-global (+TEXTURE0+ +TEXTURE0+) gl-enum)
(define-webgl-global (+TEXTURE1+ +TEXTURE1+) gl-enum)
(define-webgl-global (+TEXTURE2+ +TEXTURE2+) gl-enum)
(define-webgl-global (+TEXTURE3+ +TEXTURE3+) gl-enum)
(define-webgl-global (+TEXTURE4+ +TEXTURE4+) gl-enum)
(define-webgl-global (+TEXTURE5+ +TEXTURE5+) gl-enum)
(define-webgl-global (+TEXTURE6+ +TEXTURE6+) gl-enum)
(define-webgl-global (+TEXTURE7+ +TEXTURE7+) gl-enum)
(define-webgl-global (+TEXTURE8+ +TEXTURE8+) gl-enum)
(define-webgl-global (+TEXTURE9+ +TEXTURE9+) gl-enum)
(define-webgl-global (+TEXTURE10+ +TEXTURE10+) gl-enum)
(define-webgl-global (+TEXTURE11+ +TEXTURE11+) gl-enum)
(define-webgl-global (+TEXTURE12+ +TEXTURE12+) gl-enum)
(define-webgl-global (+TEXTURE13+ +TEXTURE13+) gl-enum)
(define-webgl-global (+TEXTURE14+ +TEXTURE14+) gl-enum)
(define-webgl-global (+TEXTURE15+ +TEXTURE15+) gl-enum)
(define-webgl-global (+TEXTURE16+ +TEXTURE16+) gl-enum)
(define-webgl-global (+TEXTURE17+ +TEXTURE17+) gl-enum)
(define-webgl-global (+TEXTURE18+ +TEXTURE18+) gl-enum)
(define-webgl-global (+TEXTURE19+ +TEXTURE19+) gl-enum)
(define-webgl-global (+TEXTURE20+ +TEXTURE20+) gl-enum)
(define-webgl-global (+TEXTURE21+ +TEXTURE21+) gl-enum)
(define-webgl-global (+TEXTURE22+ +TEXTURE22+) gl-enum)
(define-webgl-global (+TEXTURE23+ +TEXTURE23+) gl-enum)
(define-webgl-global (+TEXTURE24+ +TEXTURE24+) gl-enum)
(define-webgl-global (+TEXTURE25+ +TEXTURE25+) gl-enum)
(define-webgl-global (+TEXTURE26+ +TEXTURE26+) gl-enum)
(define-webgl-global (+TEXTURE27+ +TEXTURE27+) gl-enum)
(define-webgl-global (+TEXTURE28+ +TEXTURE28+) gl-enum)
(define-webgl-global (+TEXTURE29+ +TEXTURE29+) gl-enum)
(define-webgl-global (+TEXTURE30+ +TEXTURE30+) gl-enum)
(define-webgl-global (+TEXTURE31+ +TEXTURE31+) gl-enum)
(define-webgl-global (+ACTIVE_TEXTURE+ +ACTIVE-TEXTURE+) gl-enum)
(define-webgl-global (+REPEAT+ +REPEAT+) gl-enum)
(define-webgl-global (+CLAMP_TO_EDGE+ +CLAMP-TO-EDGE+) gl-enum)
(define-webgl-global (+MIRRORED_REPEAT+ +MIRRORED-REPEAT+) gl-enum)
(define-webgl-global (+FLOAT_VEC2+ +FLOAT-VEC2+) gl-enum)
(define-webgl-global (+FLOAT_VEC3+ +FLOAT-VEC3+) gl-enum)
(define-webgl-global (+FLOAT_VEC4+ +FLOAT-VEC4+) gl-enum)
(define-webgl-global (+INT_VEC2+ +INT-VEC2+) gl-enum)
(define-webgl-global (+INT_VEC3+ +INT-VEC3+) gl-enum)
(define-webgl-global (+INT_VEC4+ +INT-VEC4+) gl-enum)
(define-webgl-global (+BOOL+ +BOOL+) gl-enum)
(define-webgl-global (+BOOL_VEC2+ +BOOL-VEC2+) gl-enum)
(define-webgl-global (+BOOL_VEC3+ +BOOL-VEC3+) gl-enum)
(define-webgl-global (+BOOL_VEC4+ +BOOL-VEC4+) gl-enum)
(define-webgl-global (+FLOAT_MAT2+ +FLOAT-MAT2+) gl-enum)
(define-webgl-global (+FLOAT_MAT3+ +FLOAT-MAT3+) gl-enum)
(define-webgl-global (+FLOAT_MAT4+ +FLOAT-MAT4+) gl-enum)
(define-webgl-global (+SAMPLER_2D+ +SAMPLER-2D+) gl-enum)
(define-webgl-global (+SAMPLER_CUBE+ +SAMPLER-CUBE+) gl-enum)
(define-webgl-global (+VERTEX_ATTRIB_ARRAY_ENABLED+ +VERTEX-ATTRIB-ARRAY-ENABLED+) gl-enum)
(define-webgl-global (+VERTEX_ATTRIB_ARRAY_SIZE+ +VERTEX-ATTRIB-ARRAY-SIZE+) gl-enum)
(define-webgl-global (+VERTEX_ATTRIB_ARRAY_STRIDE+ +VERTEX-ATTRIB-ARRAY-STRIDE+) gl-enum)
(define-webgl-global (+VERTEX_ATTRIB_ARRAY_TYPE+ +VERTEX-ATTRIB-ARRAY-TYPE+) gl-enum)
(define-webgl-global (+VERTEX_ATTRIB_ARRAY_NORMALIZED+ +VERTEX-ATTRIB-ARRAY-NORMALIZED+) gl-enum)
(define-webgl-global (+VERTEX_ATTRIB_ARRAY_POINTER+ +VERTEX-ATTRIB-ARRAY-POINTER+) gl-enum)
(define-webgl-global (+VERTEX_ATTRIB_ARRAY_BUFFER_BINDING+ +VERTEX-ATTRIB-ARRAY-BUFFER-BINDING+) gl-enum)
(define-webgl-global (+COMPILE_STATUS+ +COMPILE-STATUS+) gl-enum)
(define-webgl-global (+LOW_FLOAT+ +LOW-FLOAT+) gl-enum)
(define-webgl-global (+MEDIUM_FLOAT+ +MEDIUM-FLOAT+) gl-enum)
(define-webgl-global (+HIGH_FLOAT+ +HIGH-FLOAT+) gl-enum)
(define-webgl-global (+LOW_INT+ +LOW-INT+) gl-enum)
(define-webgl-global (+MEDIUM_INT+ +MEDIUM-INT+) gl-enum)
(define-webgl-global (+HIGH_INT+ +HIGH-INT+) gl-enum)
(define-webgl-global (+FRAMEBUFFER+ +FRAMEBUFFER+) gl-enum)
(define-webgl-global (+RENDERBUFFER+ +RENDERBUFFER+) gl-enum)
(define-webgl-global (+RGBA4+ +RGBA4+) gl-enum)
(define-webgl-global (+RGB5_A1+ +RGB5-A1+) gl-enum)
(define-webgl-global (+RGB565+ +RGB565+) gl-enum)
(define-webgl-global (+DEPTH_COMPONENT16+ +DEPTH-COMPONENT16+) gl-enum)
(define-webgl-global (+STENCIL_INDEX+ +STENCIL-INDEX+) gl-enum)
(define-webgl-global (+STENCIL_INDEX8+ +STENCIL-INDEX8+) gl-enum)
(define-webgl-global (+DEPTH_STENCIL+ +DEPTH-STENCIL+) gl-enum)
(define-webgl-global (+RENDERBUFFER_WIDTH+ +RENDERBUFFER-WIDTH+) gl-enum)
(define-webgl-global (+RENDERBUFFER_HEIGHT+ +RENDERBUFFER-HEIGHT+) gl-enum)
(define-webgl-global (+RENDERBUFFER_INTERNAL_FORMAT+ +RENDERBUFFER-INTERNAL-FORMAT+) gl-enum)
(define-webgl-global (+RENDERBUFFER_RED_SIZE+ +RENDERBUFFER-RED-SIZE+) gl-enum)
(define-webgl-global (+RENDERBUFFER_GREEN_SIZE+ +RENDERBUFFER-GREEN-SIZE+) gl-enum)
(define-webgl-global (+RENDERBUFFER_BLUE_SIZE+ +RENDERBUFFER-BLUE-SIZE+) gl-enum)
(define-webgl-global (+RENDERBUFFER_ALPHA_SIZE+ +RENDERBUFFER-ALPHA-SIZE+) gl-enum)
(define-webgl-global (+RENDERBUFFER_DEPTH_SIZE+ +RENDERBUFFER-DEPTH-SIZE+) gl-enum)
(define-webgl-global (+RENDERBUFFER_STENCIL_SIZE+ +RENDERBUFFER-STENCIL-SIZE+) gl-enum)
(define-webgl-global (+FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE+ +FRAMEBUFFER-ATTACHMENT-OBJECT-TYPE+) gl-enum)
(define-webgl-global (+FRAMEBUFFER_ATTACHMENT_OBJECT_NAME+ +FRAMEBUFFER-ATTACHMENT-OBJECT-NAME+) gl-enum)
(define-webgl-global (+FRAMEBUFFER_ATTACHMENT_TEXTURE_LEVEL+ +FRAMEBUFFER-ATTACHMENT-TEXTURE-LEVEL+) gl-enum)
(define-webgl-global (+FRAMEBUFFER_ATTACHMENT_TEXTURE_CUBE_MAP_FACE+ +FRAMEBUFFER-ATTACHMENT-TEXTURE-CUBE-MAP-FACE+) gl-enum)
(define-webgl-global (+COLOR_ATTACHMENT0+ +COLOR-ATTACHMENT0+) gl-enum)
(define-webgl-global (+DEPTH_ATTACHMENT+ +DEPTH-ATTACHMENT+) gl-enum)
(define-webgl-global (+STENCIL_ATTACHMENT+ +STENCIL-ATTACHMENT+) gl-enum)
(define-webgl-global (+DEPTH_STENCIL_ATTACHMENT+ +DEPTH-STENCIL-ATTACHMENT+) gl-enum)
(define-webgl-global (+NONE+ +NONE+) gl-enum)
(define-webgl-global (+FRAMEBUFFER_COMPLETE+ +FRAMEBUFFER-COMPLETE+) gl-enum)
(define-webgl-global (+FRAMEBUFFER_INCOMPLETE_ATTACHMENT+ +FRAMEBUFFER-INCOMPLETE-ATTACHMENT+) gl-enum)
(define-webgl-global (+FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT+ +FRAMEBUFFER-INCOMPLETE-MISSING-ATTACHMENT+) gl-enum)
(define-webgl-global (+FRAMEBUFFER_INCOMPLETE_DIMENSIONS+ +FRAMEBUFFER-INCOMPLETE-DIMENSIONS+) gl-enum)
(define-webgl-global (+FRAMEBUFFER_UNSUPPORTED+ +FRAMEBUFFER-UNSUPPORTED+) gl-enum)
(define-webgl-global (+FRAMEBUFFER_BINDING+ +FRAMEBUFFER-BINDING+) gl-enum)
(define-webgl-global (+RENDERBUFFER_BINDING+ +RENDERBUFFER-BINDING+) gl-enum)
(define-webgl-global (+MAX_RENDERBUFFER_SIZE+ +MAX-RENDERBUFFER-SIZE+) gl-enum)
(define-webgl-global (+INVALID_FRAMEBUFFER_OPERATION+ +INVALID-FRAMEBUFFER-OPERATION+) gl-enum)
(define-webgl-global (+UNPACK_FLIP_Y_WEBGL+ +UNPACK-FLIP-Y-WEBGL+) gl-enum)
(define-webgl-global (+UNPACK_PREMULTIPLY_ALPHA_WEBGL+ +UNPACK-PREMULTIPLY-ALPHA-WEBGL+) gl-enum)
(define-webgl-global (+CONTEXT_LOST_WEBGL+ +CONTEXT-LOST-WEBGL+) gl-enum)
(define-webgl-global (+UNPACK_COLORSPACE_CONVERSION_WEBGL+ +UNPACK-COLORSPACE-CONVERSION-WEBGL+) gl-enum)
(define-webgl-global (+BROWSER_DEFAULT_WEBGL+ +BROWSER-DEFAULT-WEBGL+) gl-enum)

