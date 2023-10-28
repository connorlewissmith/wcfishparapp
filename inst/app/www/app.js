$(document).ready(function(){
  $(document).on('click', '[id$=zoom_in]', function() {
    var img = $(this).closest('.tab-pane').find('.img-zoom');
    var currentZoom = img.css("zoom");
    img.css('zoom', parseFloat(currentZoom) + 0.1);
  });

  $(document).on('click', '[id$=zoom_out]', function() {
    var img = $(this).closest('.tab-pane').find('.img-zoom');
    var currentZoom = img.css("zoom");
    img.css('zoom', parseFloat(currentZoom) - 0.1);
  });
});
