!
!     (c) 2019-2020 Guide Star Engineering, LLC
!     This Software was developed for the US Nuclear Regulatory Commission (US NRC) under contract
!     "Multi-Dimensional Physics Implementation into Fuel Analysis under Steady-state and Transients (FAST)",
!     contract # NRC-HQ-60-17-C-0007
!
submodule(array_functions_interface) array_functions_implementation
  use assert_m, only : assert
  implicit none
contains

  module procedure column_vectors
      integer i, j, k

      associate( n => shape(vector_field) )
        call assert(size(n)==4, "3D vector field input")
        allocate( array_of_3D_column_vectors( n(4), product(n(1:3)) ) )
        do concurrent( i=1:n(1), j=1:n(2), k=1:n(3) )
          associate( id => (k-1)*PRODUCT(n(1:2)) + (j-1)*n(1) + i )
            array_of_3D_column_vectors(:,id) =  vector_field(i,j,k,:)
          end associate
        end do
      end associate

  end procedure

  module procedure concatenate_columns
    !! Using reshape rather than manipulating array elements directly frees the compiler to decide the particular order of array
    !! element references that best exploits the given platform.  Alternatively, do concurrent could instead free the compiler
    !! to order element accesses however is best. Trade-off: reshape requires the creation of temporary array results but reshape
    !! is likely to have more mature compiler support than do concurrent.  If this code turns out to be a critical performance
    !! bottleneck, try replacing this implementation with element-by-element copying using do concurrent.
    associate(rows=>size(a,1))
    associate(cols=>size(a,2)+size(b,2))
    associate(a_unrolled=>reshape(a,[size(a)]))
    associate(b_unrolled=>reshape(b,[size(b)]))
      call assert( rows==size(b,1), "array_functions: compatible shapes")
      concatenated = reshape( [a_unrolled, b_unrolled ],[rows, cols] )
    end associate; end associate; end associate; end associate
  end procedure

  module procedure concatenate_rows
    !! For simplicity, this implementation invokes concatenate_columns at the cost of transpose creating additional temporaries.
    !! If this code turns out to be a critical performance bottleneck, try replacing this implementation with element-by-element
    !! copying using do concurrent.
    concatenated = transpose( concatenate_columns(transpose(a),transpose(b)) )
  end procedure

end submodule
