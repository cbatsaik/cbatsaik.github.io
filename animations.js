// File: animations.js

$(document).ready(function() {
    $(window).scroll(function() {
        $('.fade-in-on-scroll').each(function(i) {
            var bottom_of_element = $(this).offset().top + $(this).outerHeight();
            var bottom_of_window = $(window).scrollTop() + $(window).height();

            if (bottom_of_window > bottom_of_element) {
                $(this).animate({'opacity':'1'}, 1000);
            }
        });
    });
});
