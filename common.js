//main navigation

jQuery(document).ready(function($){


function equal_height() {
    $('.softwre_sec_col').jQueryEqualHeight('.softwre_sec_col_inner');
    $('.softwre_sec_col').jQueryEqualHeight('.softwre_sec_col_inner .sm-ht');
    $('.panel_data_mthd_col').jQueryEqualHeight('.panel_data_mthd_col_inner .hdng');
    $('.panel_data_mthd_col').jQueryEqualHeight('.panel_data_mthd_col_inner');
}
$(window).on('load', function(event) {
    equal_height();
});
$(window).resize(function(event) {
    equal_height();
});
	// Recalculate heights on tab switch
$('.tab_area_part_main .nav-tabs a').on('shown.bs.tab', function() {
        equal_height(); // Manually call equal_height when the tab is shown
    });
	
	
// Navbar
$( "<span class='clickD'></span>" ).insertAfter(".navbar-nav li.menu-item-has-children > a");
 $('.navbar-nav li .clickD').click(function(e) {
    e.preventDefault();
    var $this = $(this);
  
    if ($this.next().hasClass('show')) {
        $this.next().removeClass('show');
        $this.removeClass('toggled');
    } else {
        $this.parent().parent().find('.sub-menu').removeClass('show');
        $this.parent().parent().find('.toggled').removeClass('toggled');
        $this.next().toggleClass('show');
        $this.toggleClass('toggled');
    }
});

jQuery(window).on('resize', function(){
    var win = $(this); //this = window
    if (win.width() < 1025) {
        $('html').click(function(){
            $('.navbar-nav li .clickD').removeClass('toggled');
            $('.toggled').removeClass('toggled');
            $('.sub-menu').removeClass('show');
        });
        $(document).click(function(){
            $('.navbar-nav li .clickD').removeClass('toggled');
            $('.toggled').removeClass('toggled');
            $('.sub-menu').removeClass('show');
        });
        $('.navbar-nav').click(function(e){
        e.stopPropagation();
        });
     }
});
// Navbar end



$(".search_icoo_head_out").click(function(){
    $(".main-head-botttom").addClass("actv_part");
    $("body").addClass("actv_part");
});

$(".hd_meu_close").click(function(){
   $(".main-head-botttom").removeClass("actv_part");
   $("body").removeClass("actv_part");
});

//equal_height();

// aos
AOS.init({
  disable: function() {
    var maxWidth = 1025;
    return window.innerWidth < maxWidth;
  }
});

if( $('.tab_area_part_main').length ){

 $('.tab_area_part_main .nav-tabs').responsiveTabs();

}

$(".main-head-top .navbar-toggler").click(function(){
    $("body").toggleClass("show_hd_bdy");
});

$(".mobile-menu-close.mobileMenuClose").click(function(){
  $(".navbar-collapse").removeClass("show");
  $("body").removeClass("show_hd_bdy");
});


$(".sub-menu > li > a").click(function(){
    var writingheight = $(".main-head").height();
    //alert(writingheight);
    var offset = $(':target').offset();
   var scrollto = parseFloat(offset.top) - parseFloat(writingheight); // minus fixed header height
   //console.log(offset);
   //console.log(writingheight);
   //console.log(scrollto);
    $('html, body').animate({scrollTop:scrollto}, 0);
});

    // var offset = $(':target').offset();
    // var scrollto = offset.top - 200; // minus fixed header height

    // $('html, body').animate({scrollTop:scrollto}, 0);
$('.read-more-btn').on('click', function() {
        $(this).toggleClass('expanded');
        if ($(this).hasClass('expanded')) {
            $(this).text('Read Less');
            $(this).siblings('.content').css('max-height', 'none');
        } else {
            $(this).text('Read More');
            $(this).siblings('.content').css('max-height', '150px');
        }
    });

});

// document ready end







