pro month_lat_view, zmolat, _EXTRA=e

common axes		; in : depth
common comscale		; in : scale
common dataranges	; in : idp0, idp1, ip0, ip1
common plots_info	; in : titles

WIDGET_CONTROL, /HOURGLASS

set_xymargins, junk, my_ymargin

values = zmolat.matrix(idp0:idp1,ip0:ip1)
               
contour, zmolat.matrix(idp0:idp1,ip0:ip1), depth.coord(idp0:idp1),    $
      x.coord(ip0:ip1),                                               $
      level=scale.vector, ymargin=my_ymargin,                         $
      title=titles(0)+'!C'+titles(1),                                 $
      subtitle=titles(2)+'!C!C'+titles(3), /follow,                   $
      xrange=[depth.coord(idp0),depth.coord(idp1)], xstyle=1,         $
      ytitle=x.name, yrange=[x.coord(ip0),x.coord(ip1)], ystyle=1,    $
      xtitle=depth.label.name, xticks=depth.label.nb, ticklen=-0.015, $
      xtickname=depth.label.txt(0:depth.label.nb),                    $
      xtickv=depth.label.coord(0:depth.label.nb), _EXTRA=e

return
end
