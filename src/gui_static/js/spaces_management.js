// ===================================================================
// @author Krzysztof Trzepla
// @copyright (C): 2016 ACK CYFRONET AGH
// This software is released under the MIT license
// cited in 'LICENSE.txt'.
// @end
// ===================================================================
// @doc: This file contains utility functions used on spaces management page.
// @end
// ===================================================================

// Returns panel for space size selection.
function space_size_panel() {
    return '<div id="space_size_div" style="width: 100%;">' +
        '<input id="space_size" class="pull-left" type="text" style="width: 50%;" placeholder="Support size">' +
        '<fieldset class="pull-right" style="width: 41%; line-height: 40px;">' +
        '<label id="size_mb" class="radio checked" style="display: inline-block; padding-left: 25px; margin-right: 10px;">' +
        '<input type="radio" name="optionsRadios2" id="optionsRadios1" value="option1" data-toggle="radio" checked="checked">MB' +
        '</label>' +
        '<label id="size_gb" class="radio" style="display: inline-block; padding-left: 25px; margin-right: 10px;">' +
        '<input type="radio" name="optionsRadios2" id="optionsRadios2" value="option1" data-toggle="radio">GB' +
        '</label>' +
        '<label id="size_tb" class="radio" style="display: inline-block; padding-left: 25px; margin-right: 10px;">' +
        '<input type="radio" name="optionsRadios2" id="optionsRadios3" value="option1" data-toggle="radio">TB' +
        '</label>' +
        '</fieldset>' +
        '</div>'
}

// Initializes FlatUI radio buttons.
function initialize_radio_buttons() {
    $('[data-toggle="radio"]').each(function () {
        var $radio = $(this);
        $radio.radio();
    });
}

// Renders storage support form.
function storage_support_form() {
    $('#storage_list').after(
        '<div id="storage_form">' +
        '<input id="space_token" type="text" style="width: 100%;" placeholder="Space token">' +
        space_size_panel() +
        '</div>'
    );
    initialize_radio_buttons();
    $('#space_token').focus();
}

// Renders storage create form.
function storage_create_form() {
    $('#storage_list').after(
        '<div id="storage_form">' +
        '<input id="space_name" type="text" style="width: 100%;" placeholder="Space name">' +
        '<input id="space_token" type="text" style="width: 100%;" placeholder="Space token">' +
        space_size_panel() +
        '</div>'
    );
    initialize_radio_buttons();
    $('#space_name').focus();
}

// Validates provided parameter.
function check_params(storage_id, token, size) {
    var message = $('#space_alert');
    if(storage_id == undefined) {
        message.html('Please configure storage first.');
        message.fadeIn(300);
        return false;
    }
    if (token.length == 0) {
        message.html('Please provide Space token.');
        message.fadeIn(300);
        return false;
    }
    if (size.length == 0) {
        message.html('Please provide Space size.');
        message.fadeIn(300);
        return false;
    }
    if (isNaN(size) || parseInt(size, 10) <= 0) {
        message.html('Space size should be a positive number.');
        message.fadeIn(300);
        return false;
    }
    return true;
}

// Multiplies space size parameter according to selected unit.
function multiply_space_size(size) {
    if ($('#size_mb').hasClass('checked')) {
        return 1024 * 1024 * size;
    }
    if ($('#size_gb').hasClass('checked')) {
        return 1024 * 1024 * 1024 * size;
    }
    return 1024 * 1024 * 1024 * 1024 * size;
}

// Validates space create parameters.
function create_space_check() {
    var message = $('#space_alert');
    var storage_id = $('#storage_id').val();
    var name = $.trim($('#space_name').val());
    var token = $.trim($('#space_token').val());
    var size = $.trim($('#space_size').val());
    if (name.length == 0) {
        message.html('Please provide Space name.');
        message.fadeIn(300);
        return false;
    }
    if (!check_params(storage_id ,token, size)) {
        return false;
    }
    size = multiply_space_size(size);
    create_space([storage_id, name, token, size]);
    return true;
}

// Validates space support parameters.
function support_space_check() {
    var storage_id = $('#storage_id').val();
    var token = $.trim($('#space_token').val());
    var size = $.trim($('#space_size').val());
    if (!check_params(storage_id, token, size)) {
        return false;
    }
    size = multiply_space_size(size);
    support_space([storage_id, token, size]);
    return true;
}