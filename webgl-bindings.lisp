(in-package #:webgl-bindings)

(defparameter *webgl-context* '*gl*
  "name of variable used to store GL context object, used to convert
for example (gl:clear) -> GL.clear(), can be overridden locally
using (with-gl-context (name) ...) from ps code, or rebound globally
before compiling PS code using this library.")

(ps:defpsmacro with-gl-context ((var) &body body)
  `(symbol-macrolet ((*webgl-context* ,var))
     ,@body))

(defmacro define-webgl-fun (&whole w name ret &body args)
  "define gl:foo psmacros to expand to calls to the corresponding
method on *webgl-context*. If there is a PS symbol macro named
*webgl-context* (as defined by with-gl-context), it should expand to a
var containing the context, otherwise the value of the CL special
*webgl-context* will be used."
  ;; TODO: probably should define a symbol-macro to access the value
  ;;   of the members too? (for passing functions to other functions, etc)
  (declare (ignore ret))
  `(ps:defpsmacro ,name ,(loop for (nil name) in args
                            collect name)
     (let* ((local-context (ps::ps-macroexpand '*webgl-context*))
            (context (if (eq local-context '*webgl-context*)
                         *webgl-context*
                         local-context)))
       `(funcall (ps:@ ,context ,',name)
                 ,,@(loop for (nil name) in args
                       collect name)))))


;;;; attributes
(defpsmacro %webgl-global (name)
  (let* ((local-context (ps::ps-macroexpand '*webgl-context*))
         (context (if (eq local-context '*webgl-context*)
                      *webgl-context*
                      local-context)))
    `(ps:@ ,context ,name)))

(defmacro define-webgl-global (name type)
  "define gl:foo ps symbol-macros to expand to corresponding attribute
on *webgl-context*. If there is a PS symbol macro named
*webgl-context* (as defined by with-gl-context), it should expand to a
var containing the context, otherwise the value of the CL special
*webgl-context* will be used."
  (declare (ignore type))
  `(ps:define-ps-symbol-macro ,name
       (%webgl-global ,name)))

(define-webgl-global canvas html-canvas-element)
(define-webgl-global drawing-buffer-width glsizei)
(define-webgl-global drawing-buffer-height glsizei)

;;;; functions
(define-webgl-fun get-context-attributes webgl-context-attributes
  )
(define-webgl-fun is-context-lost boolean
  )

(define-webgl-fun get-supported-extensions dom-string[]
  )
(define-webgl-fun get-extension object
  (dom-string name))

(define-webgl-fun active-texture void
  (glenum texture))
(define-webgl-fun attach-shader void
  (web-gl-program  program)
  (web-gl-shader shader))
(define-webgl-fun bind-attrib-location void
  (web-gl-program program)
  (gl-uint index)
  (domstring name))
(define-webgl-fun bind-buffer void
  (gl-enum target)
  (webgl-buffer buffer))
(define-webgl-fun bind-framebuffer void
  (gl-enum target)
  (webgl-framebuffer framebuffer))
(define-webgl-fun bind-renderbuffer void
  (gl-enum target)
  (webgl-renderbuffer renderbuffer))
(define-webgl-fun bind-texture void
  (gl-enum target)
  (webgl-texture texture))
(define-webgl-fun blend-color void
  (gl-clampf red)
  (gl-clampf green)
  (gl-clampf blue)
  (gl-clampf alpha))
(define-webgl-fun blend-equation void
  (gl-enum mode))
(define-webgl-fun blend-equation-separate void
  (gl-enum mode-rgb)
  (gl-enum mode-alpha))
(define-webgl-fun blend-func void
  (gl-enum sfactor)
  (gl-enum dfactor))
(define-webgl-fun blend-func-separate void
  (gl-enum src-rgb)
  (gl-enum dst-rgb)
  (gl-enum src-alpha)
  (gl-enum dst-alpha))

(define-webgl-fun buffer-data void
  (gl-enum target)
  (gl-sizeiptr size)
  (gl-enum usage))
#++
(define-webgl-fun buffer-data void
  (gl-enum target)
  (array-buffer-view data)
  (gl-enum usage))
#++
(define-webgl-fun buffer-data void
  (gl-enum target)
  (array-buffer data)
  (gl-enum usage))
(define-webgl-fun buffer-sub-data void
  (gl-enum target)
  (gl-intptr offset)
  (array-buffer-view data))
#++
(define-webgl-fun buffer-sub-data void
  (gl-enum target)
  (gl-intptr offset)
  (array-buffer data))

(define-webgl-fun check-framebuffer-status gl-enum
  (gl-enum target))
(define-webgl-fun clear void
  (gl-bitfield mask))
(define-webgl-fun clear-color void
  (gl-clampf red)
  (gl-clampf green)
  (gl-clampf blue)
  (gl-clampf alpha))

(define-webgl-fun clear-depth void
  (gl-clampf depth))
(define-webgl-fun clear-stencil void
  (gl-int s))
(define-webgl-fun color-mask void
  (gl-boolean red)
  (gl-boolean green)
  (gl-boolean blue)
  (gl-boolean alpha))
(define-webgl-fun compile-shader void
  (webgl-shader shader))

(define-webgl-fun copy-tex-image2d void
  (gl-enum target)
  (gl-int level)
  (gl-enum internalformat)
  (gl-int x)
  (gl-int y)
  (gl-sizei width)
  (gl-sizei height)
  (gl-int border))
(define-webgl-fun copy-tex-sub-image-2d void
  (gl-enum target)
  (gl-int level)
  (gl-int xoffset)
  (gl-int yoffset)
  (gl-int x)
  (gl-int y)
  (gl-sizei width)
  (gl-sizei height))

(define-webgl-fun create-buffer webgl-buffer
  )
(define-webgl-fun create-framebuffer webgl-framebuffer
  )
(define-webgl-fun create-program webgl-program
  )
(define-webgl-fun create-renderbuffer webgl-renderbuffer
  )
(define-webgl-fun create-shader webgl-shader
  (gl-enum type))
(define-webgl-fun create-texture webgl-texture
  )

(define-webgl-fun cull-face void
  (gl-enum mode))

(define-webgl-fun delete-buffer void
  (webgl-buffer buffer))
(define-webgl-fun delete-framebuffer void
  (webgl-framebuffer framebuffer))
(define-webgl-fun delete-program void
  (webgl-program program))
(define-webgl-fun delete-renderbuffer void
  (webgl-renderbuffer renderbuffer))
(define-webgl-fun delete-shader void
  (webgl-shader shader))
(define-webgl-fun delete-texture void
  (webgl-texture texture))

(define-webgl-fun depth-func void
  (gl-enum func))
(define-webgl-fun depth-mask void
  (gl-boolean flag))
(define-webgl-fun depth-range void
  (gl-clampf z-near)
  (gl-clampf z-far))
(define-webgl-fun detach-shader void
  (webgl-program program)
  (webgl-shader shader))
(define-webgl-fun disable void
  (gl-enum cap))
(define-webgl-fun disable-vertex-attrib-array void
  (gl-uint index))
(define-webgl-fun draw-arrays void
  (gl-enum mode)
  (gl-int first)
  (gl-sizei count))
(define-webgl-fun draw-elements void
  (gl-enum mode)
  (gl-sizei count)
  (gl-enum type)
  (gl-intptr offset))

(define-webgl-fun enable void
  (gl-enum cap))
(define-webgl-fun enable-vertex-attrib-array void
  (gl-uint index))
(define-webgl-fun finish void
  )
(define-webgl-fun flush void
  )
(define-webgl-fun framebuffer-renderbuffer void
  (gl-enum target)
  (gl-enum attachment)
  (gl-enum renderbuffertarget)
  (webgl-renderbuffer renderbuffer))
(define-webgl-fun framebuffer-texture-2d void
  (gl-enum target)
  (gl-enum attachment)
  (gl-enum textarget)
  (webgl-texture texture)
  (gl-int level))
(define-webgl-fun front-face void
  (gl-enum mode))
(define-webgl-fun generate-mipmap void
  (gl-enum target))

(define-webgl-fun get-active-attrib webgl-active-info
  (webgl-program program)
  (gl-uint index))
(define-webgl-fun get-active-uniform webgl-active-info
  (webgl-program program)
  (gl-uint index))
(define-webgl-fun get-attached-shaders webgl-shader[]
  (webgl-program program))

(define-webgl-fun get-attrib-location gl-int
  (webgl-program program)
  (domstring name))

(define-webgl-fun get-parameter any
  (gl-enum pname))
(define-webgl-fun get-buffer-parameter any
  (gl-enum target)
  (gl-enum pname))

(define-webgl-fun get-error gl-enum
  )

(define-webgl-fun get-framebuffer-attachment-parameter any
  (gl-enum target)
  (gl-enum attachment)
  (gl-enum pname))
(define-webgl-fun get-program-parameter any
  (webgl-program program)
  (gl-enum pname))
(define-webgl-fun get-program-info-log domstring
  (webgl-program program))
(define-webgl-fun get-renderbuffer-parameter any
  (gl-enum target)
  (gl-enum pname))
(define-webgl-fun get-shader-parameter any
  (webgl-shader shader)
  (gl-enum pname))
(define-webgl-fun get-shader-info-log domstring
  (webgl-shader shader))

(define-webgl-fun get-shader-source domstring
  (webgl-shader shader))

(define-webgl-fun get-tex-parameter any
  (gl-enum target)
  (gl-enum pname))

(define-webgl-fun get-uniform any
  (webgl-program program)
  (webgl-uniform-location location))

(define-webgl-fun get-uniform-location webgl-uniform-location
  (webgl-program program)
  (domstring name))

(define-webgl-fun get-vertex-attrib any
  (gl-uint index)
  (gl-enum pname))

(define-webgl-fun get-vertex-attrib-offset gl-sizeiptr
  (gl-uint index)
  (gl-enum pname))

(define-webgl-fun hint void
  (gl-enum target)
  (gl-enum mode))
(define-webgl-fun is-buffer gl-boolean
  (webgl-buffer buffer))
(define-webgl-fun is-enabled gl-boolean
  (gl-enum cap))
(define-webgl-fun is-framebuffer gl-boolean
  (webgl-framebuffer framebuffer))
(define-webgl-fun is-program gl-boolean
  (webgl-program program))
(define-webgl-fun is-renderbuffer gl-boolean
  (webgl-renderbuffer renderbuffer))
(define-webgl-fun is-shader gl-boolean
  (webgl-shader shader))
(define-webgl-fun is-texture gl-boolean
  (webgl-texture texture))
(define-webgl-fun line-width void
  (gl-float width))
(define-webgl-fun link-program void
  (webgl-program program))
(define-webgl-fun pixel-store-i void
  (gl-enum pname)
  (gl-int param))
(define-webgl-fun polygon-offset void
  (gl-float factor)
  (gl-float units))

(define-webgl-fun read-pixels void
  (gl-int x)
  (gl-int y)
  (gl-sizei width)
  (gl-sizei height)
  (gl-enum format)
  (gl-enum type)
  (array-buffer-view pixels))

(define-webgl-fun renderbuffer-storage void
  (gl-enum target)
  (gl-enum internalformat)
  (gl-sizei width)
  (gl-sizei height))
(define-webgl-fun sample-coverage void
  (gl-clampf value)
  (gl-boolean invert))
(define-webgl-fun scissor void
  (gl-int x)
  (gl-int y)
  (gl-sizei width)
  (gl-sizei height))

(define-webgl-fun shader-source void
  (webgl-shader shader)
  (domstring source))

(define-webgl-fun stencil-func void
  (gl-enum func)
  (gl-int ref)
  (gl-uint mask))
(define-webgl-fun stencil-func-separate void
  (gl-enum face)
  (gl-enum func)
  (gl-int ref)
  (gl-uint mask))
(define-webgl-fun stencil-mask void
  (gl-uint mask))
(define-webgl-fun stencil-mask-separate void
  (gl-enum face)
  (gl-uint mask))
(define-webgl-fun stencil-op void
  (gl-enum fail)
  (gl-enum zfail)
  (gl-enum zpass))
(define-webgl-fun stencil-op-separate void
  (gl-enum face)
  (gl-enum fail)
  (gl-enum zfail)
  (gl-enum zpass))

(define-webgl-fun tex-image-2d void
  (gl-enum target)
  (gl-int level)
  (gl-enum internalformat)
  (gl-sizei width)
  (gl-sizei height)
  (gl-int border)
  (gl-enum format)
  (gl-enum type)
  (array-buffer-view pixels))
#++
(define-webgl-fun tex-image-2d void
  (gl-enum target)
  (gl-int level)
  (gl-enum internalformat)
  (gl-enum format)
  (gl-enum type)
  (image-data pixels))
#++
(define-webgl-fun tex-image2d void
  (gl-enum target)
  (gl-int level)
  (gl-enum internalformat)
  (gl-enum format)
  (gl-enum type)
  (html-image-element image))
#++
(define-webgl-fun tex-image2d void
  (gl-enum target)
  (gl-int level)
  (gl-enum internalformat)
  (gl-enum format)
  (gl-enum type)
  (html-canvas-element canvas))
#++
(define-webgl-fun tex-image2d void
  (gl-enum target)
  (gl-int level)
  (gl-enum internalformat)
  (gl-enum format)
  (gl-enum type)
  (html-video-element video))

(define-webgl-fun tex-parameter-f void
  (gl-enum target)
  (gl-enum pname)
  (gl-float param))
(define-webgl-fun tex-parameter-i void
  (gl-enum target)
  (gl-enum pname)
  (gl-int param))

(define-webgl-fun tex-sub-image-2d void
  (gl-enum target)
  (gl-int level)
  (gl-int xoffset)
  (gl-int yoffset)
  (gl-sizei width)
  (gl-sizei height)
  (gl-enum format)
  (gl-enum type)
  (array-buffer-view pixels))
#++
(define-webgl-fun tex-sub-image-2d void
  (gl-enum target)
  (gl-int level)
  (gl-int xoffset)
  (gl-int yoffset)
  (gl-enum format)
  (gl-enum type)
  (image-data pixels))
#++
(define-webgl-fun tex-sub-image-2d void
  (gl-enum target)
  (gl-int level)
  (gl-int xoffset)
  (gl-int yoffset)
  (gl-enum format)
  (gl-enum type)
  (html-image-element image))
#++
(define-webgl-fun tex-sub-image-2d void
  (gl-enum target)
  (gl-int level)
  (gl-int xoffset)
  (gl-int yoffset)
  (gl-enum format)
  (gl-enum type)
  (html-canvas-element canvas))
#++
(define-webgl-fun tex-sub-image-2d void
  (gl-enum target)
  (gl-int level)
  (gl-int xoffset)
  (gl-int yoffset)
  (gl-enum format)
  (gl-enum type)
  (html-video-element video))

(define-webgl-fun uniform-1f void
  (webgl-uniform-location location)
  (gl-float x))
(define-webgl-fun uniform-1fv void
  (webgl-uniform-location location)
  (float32-array v))
#++
(define-webgl-fun uniform-1fv void
  (webgl-uniform-location location)
  (float[] v))
(define-webgl-fun uniform-1i void
  (webgl-uniform-location location)
  (gl-int x))
(define-webgl-fun uniform-1iv void
  (webgl-uniform-location location)
  (int32-array v))
#++
(define-webgl-fun uniform-1iv void
  (webgl-uniform-location location)
  (long[] v))
(define-webgl-fun uniform-2f void
  (webgl-uniform-location location)
  (gl-float x)
  (gl-float y))
(define-webgl-fun uniform-2fv void
  (webgl-uniform-location location)
  (float32-array v))
#++
(define-webgl-fun uniform-2fv void
  (webgl-uniform-location location)
  (float[] v))
(define-webgl-fun uniform-2i void
  (webgl-uniform-location location)
  (gl-int x)
  (gl-int y))
(define-webgl-fun uniform-2iv void
  (webgl-uniform-location location)
  (int32-array v))
#++
(define-webgl-fun uniform-2iv void
  (webgl-uniform-location location)
  (long[] v))
(define-webgl-fun uniform-3f void
  (webgl-uniform-location location)
  (gl-float x)
  (gl-float y)
  (gl-float z))
(define-webgl-fun uniform-3fv void
  (webgl-uniform-location location)
  (float32-array v))
#++
(define-webgl-fun uniform-3fv void
  (webgl-uniform-location location)
  (float[] v))
(define-webgl-fun uniform-3i void
  (webgl-uniform-location location)
  (gl-int x)
  (gl-int y)
  (gl-int z))
(define-webgl-fun uniform-3iv void
  (webgl-uniform-location location)
  (int32-array v))
#++
(define-webgl-fun uniform-3iv void
  (webgl-uniform-location location)
  (long[] v))
(define-webgl-fun uniform-4f void
  (webgl-uniform-location location)
  (gl-float x)
  (gl-float y)
  (gl-float z)
  (gl-float w))
(define-webgl-fun uniform-4fv void
  (webgl-uniform-location location)
  (float32-array v))
#++
(define-webgl-fun uniform4-fv void
  (webgl-uniform-location location)
  (float[] v))
(define-webgl-fun uniform-4i void
  (webgl-uniform-location location)
  (gl-int x)
  (gl-int y)
  (gl-int z)
  (gl-int w))
(define-webgl-fun uniform-4iv void
  (webgl-uniform-location location)
  (int32-array v))
#++
(define-webgl-fun uniform-4iv void
  (webgl-uniform-location location)
  (long[] v))
(define-webgl-fun uniform-matrix-2fv void
  (webgl-uniform-location location)
  (gl-boolean transpose)
  (float32-array value))
(define-webgl-fun uniform-matrix-2fv void
  (webgl-uniform-location location)
  (gl-boolean transpose)
  (float[] value))
(define-webgl-fun uniform-matrix-3fv void
  (webgl-uniform-location location)
  (gl-boolean transpose)
  (float32-array value))
(define-webgl-fun uniform-matrix-3fv void
  (webgl-uniform-location location)
  (gl-boolean transpose)
  (float[] value))
(define-webgl-fun uniform-matrix-4fv void
  (webgl-uniform-location location)
  (gl-boolean transpose)
  (float32-array value))
(define-webgl-fun uniform-matrix-4fv void
  (webgl-uniform-location location)
  (gl-boolean transpose)
  (float[] value))

(define-webgl-fun use-program void
  (webgl-program program))
(define-webgl-fun validate-program void
  (webgl-program program))

(define-webgl-fun vertex-attrib-1f void
  (gl-uint indx)
  (gl-float x))
(define-webgl-fun vertex-attrib-1fv void
  (gl-uint indx)
  (float32-array values))
#++
(define-webgl-fun vertex-attrib-1fv void
  (gl-uint indx)
  (float[] values))
(define-webgl-fun vertex-attrib-2f void
  (gl-uint indx)
  (gl-float x)
  (gl-float y))
(define-webgl-fun vertex-attrib-2fv void
  (gl-uint indx)
  (float32-array values))
#++
(define-webgl-fun vertex-attrib-2fv void
  (gl-uint indx)
  (float[] values))
(define-webgl-fun vertex-attrib-3f void
  (gl-uint indx)
  (gl-float x)
  (gl-float y)
  (gl-float z))
(define-webgl-fun vertex-attrib-3fv void
  (gl-uint indx)
  (float32-array values))
#++
(define-webgl-fun vertex-attrib-3fv void
  (gl-uint indx)
  (float[] values))
(define-webgl-fun vertex-attrib-4f void
  (gl-uint indx)
  (gl-float x)
  (gl-float y)
  (gl-float z)
  (gl-float w))
(define-webgl-fun vertex-attrib-4fv void
  (gl-uint indx)
  (float32-array values))
#++
(define-webgl-fun vertex-attrib-4fv void
  (gl-uint indx)
  (float[] values))
(define-webgl-fun vertex-attrib-pointer void
  (gl-uint indx)
  (gl-int size)
  (gl-enum type)
  (gl-boolean normalized)
  (gl-sizei stride)
  (gl-intptr offset))

(define-webgl-fun viewport void
  (gl-int x)
  (gl-int y)
  (gl-sizei width)
  (gl-sizei height))
