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

// Renders space support form for Ceph storage.
function ceph_support_form() {
    $('#storage_list').after(
        '<div id="ceph_form">' +
        '<input id="space_token" type="text" style="width: 100%;" placeholder="Space token">' +
        '<input id="ceph_username" type="text" style="width: 100%;" placeholder="Ceph username">' +
        '<input id="ceph_key" type="password" style="width: 100%;" placeholder="Ceph key">' +
        space_size_panel() +
        '</div>'
    );
    initialize_radio_buttons();
    $('#space_token').focus();
}

// Renders space create form for Ceph storage.
function ceph_create_form() {
    $('#storage_list').after(
        '<div id="ceph_form">' +
        '<input id="space_name" type="text" style="width: 100%;" placeholder="Space name">' +
        '<input id="space_token" type="text" style="width: 100%;" placeholder="Space token">' +
        '<input id="ceph_username" type="text" style="width: 100%;" placeholder="Ceph username">' +
        '<input id="ceph_key" type="password" style="width: 100%;" placeholder="Ceph key">' +
        space_size_panel() +
        '</div>'
    );
    initialize_radio_buttons();
    $('#space_name').focus();
}

// Renders space support form for direct IO storage.
function dio_support_form() {
    $('#storage_list').after(
        '<div id="dio_form">' +
        '<input id="space_token" type="text" style="width: 100%;" placeholder="Space token">' +
        space_size_panel() +
        '</div>'
    );
    initialize_radio_buttons();
    $('#space_token').focus();
}

// Renders space create form for direct IO storage.
function dio_create_form() {
    $('#storage_list').after(
        '<div id="dio_form">' +
        '<input id="space_name" type="text" style="width: 100%;" placeholder="Space name">' +
        '<input id="space_token" type="text" style="width: 100%;" placeholder="Space token">' +
        space_size_panel() +
        '</div>'
    );
    initialize_radio_buttons();
    $('#space_name').focus();
}

// Validates space size parameter.
function space_size_check(message, size) {
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
    var storage_type = $('#storage_type').val();
    var isCeph = storage_type.slice(0, 4) == 'Ceph';
    var name = $.trim($('#space_name').val());
    var token = $.trim($('#space_token').val());
    var size = $.trim($('#space_size').val());
    var username = $.trim($('#ceph_username').val());
    var key = $.trim($('#ceph_key').val());
    if (name.length == 0) {
        message.html('Please provide Space name.');
        message.fadeIn(300);
        return false;
    }
    if (token.length == 0) {
        message.html('Please provide Space token.');
        message.fadeIn(300);
        return false;
    }
    if (isCeph && username.length == 0) {
        message.html('Please provide Ceph username.');
        message.fadeIn(300);
        return false;
    }
    if (isCeph && key.length == 0) {
        message.html('Please provide Ceph key.');
        message.fadeIn(300);
        return false;
    }
    if (!space_size_check(message, size)) {
        return false;
    }
    size = multiply_space_size(size);
    create_space([storage_type, isCeph, name, token, size, username, key]);
    return true;
}

// Validates space support parameters.
function support_space_check() {
    var message = $('#space_alert');
    var storage_type = $('#storage_type').val();
    var isCeph = storage_type.slice(0, 4) == 'Ceph';
    var token = $.trim($('#space_token').val());
    var size = $.trim($('#space_size').val());
    var username = $.trim($('#ceph_username').val());
    var key = $.trim($('#ceph_key').val());
    if (token.length == 0) {
        message.html('Please provide Space token.');
        message.fadeIn(300);
        return false;
    }
    if (isCeph && username.length == 0) {
        message.html('Please provide Ceph username.');
        message.fadeIn(300);
        return false;
    }
    if (isCeph && key.length == 0) {
        message.html('Please provide Ceph key.');
        message.fadeIn(300);
        return false;
    }
    size = multiply_space_size(size);
    support_space([storage_type, isCeph, token, size, username, key]);
    return true;
}