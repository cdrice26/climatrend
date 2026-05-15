$(function() {
  $('#submit').on('click', function() {
    var btn = $(this);
    if (!btn.data('loading')) {
      btn.data('loading', true);
      btn.prop('disabled', true);
      btn.attr('style', 'display: flex; flex-direction: row; align-items: center; justify-content: center;');
      btn.attr('data-original-text', btn.html());
      btn.html('<span class="submit-spinner"></span> Loading...');
    }
  });

  $(document).on('shiny:idle', function() {
    var btn = $('#submit');
    if (btn.data('loading')) {
      btn.html(btn.attr('data-original-text'));
      btn.prop('disabled', false);
      btn.data('loading', false);
    }
  });
});
