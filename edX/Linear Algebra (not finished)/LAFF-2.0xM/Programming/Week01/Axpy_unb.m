
% Copyright 2016 The University of Texas at Austin
%
% For licensing information see
%                http://www.cs.utexas.edu/users/flame/license.html 
%                                                                                 
% Programmed by: 
%                

function [ yt_out ] = Axpy_unb( alpha, xt, yt )

  [ xLt, xRt ] = FLA_Part_1x2( xt, ...
                                 0, 'FLA_LEFT' );

  [ yLt, yRt ] = FLA_Part_1x2( yt, ...
                                 0, 'FLA_LEFT' );

  while ( size( xLt, 2 ) < size( xt, 2 ) )

    [ x0t, chi1, x2t ]= FLA_Repart_1x2_to_1x3( xLt, xRt, ...
                                             1, 'FLA_RIGHT' );

    [ y0t, psi1, y2t ]= FLA_Repart_1x2_to_1x3( yLt, yRt, ...
                                             1, 'FLA_RIGHT' );

    %------------------------------------------------------------%
psi1 = alpha * chi1 + psi1;
    %------------------------------------------------------------%

    [ xLt, xRt ] = FLA_Cont_with_1x3_to_1x2( x0t, chi1, x2t, ...
                                             'FLA_LEFT' );

    [ yLt, yRt ] = FLA_Cont_with_1x3_to_1x2( y0t, psi1, y2t, ...
                                             'FLA_LEFT' );

  end

  yt_out = [ yLt, yRt ];

return
