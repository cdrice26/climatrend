/* 
Copyright (C) 2026 Caleb Rice

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.
*/

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
