package org.lawrencebower.docgen.web_logic.business.mapping;

import org.lawrencebower.docgen.web_logic.view.document.component.DocComponentView;

public enum AutoMappedComponent {

    BUSINESS_NAME("BUSINESS_NAME"),
    BUSINESS_CONTACT_NAME("BUSINESS_CONTACT_NAME"),
    BUSINESS_ADDRESS("BUSINESS_ADDRESS"),
    BUSINESS_PHONE("BUSINESS_PHONE"),
    BUSINESS_COUNTRY("BUSINESS_COUNTRY"),

    CUSTOMER_NAME("CUSTOMER_NAME"),
    CUSTOMER_CONTACT_NAME("CUSTOMER_CONTACT_NAME"),
    CUSTOMER_ADDRESS("CUSTOMER_ADDRESS"),
    CUSTOMER_PHONE("CUSTOMER_PHONE"),
    CUSTOMER_COUNTRY("CUSTOMER_COUNTRY"),

    VENDOR_NAME("VENDOR_NAME"),
    VENDOR_CONTACT_NAME("VENDOR_CONTACT_NAME"),
    VENDOR_ADDRESS("VENDOR_ADDRESS"),
    VENDOR_PHONE("VENDOR_PHONE"),
    VENDOR_TAX_ID("VENDOR_TAX_ID"),
    VENDOR_COUNTRY("VENDOR_COUNTRY"),
    VENDOR_EMAIL("VENDOR_EMAIL");

    private String name;

    AutoMappedComponent(String name) {
        this.name = name;
    }

    public String getName() {
        return name;
    }

    public static void mapComponent(DocComponentView docComponentView,
                                    AutoMappedComponent component,
                                    AutoMappedComponentInfo info) {

        if (component == VENDOR_ADDRESS) {
            setComponentText(docComponentView, info.getVendorAddress());
        } else if (component == VENDOR_CONTACT_NAME) {
            setComponentText(docComponentView, info.getVendorContactName());
        } else if (component == VENDOR_COUNTRY) {
            setComponentText(docComponentView, info.getVendorCountry());
        } else if (component == VENDOR_NAME) {
            setComponentText(docComponentView, info.getVendorName());
        } else if (component == VENDOR_PHONE) {
            setComponentText(docComponentView, info.getVendorPhone());
        } else if (component == VENDOR_EMAIL) {
            setComponentText(docComponentView, info.getVendorEmail());
        } else if (component == VENDOR_TAX_ID) {
            setComponentText(docComponentView, info.getVendorTaxId());
        } else if (component == BUSINESS_ADDRESS) {
            setComponentText(docComponentView, info.getBusinessAddress());
        } else if (component == BUSINESS_CONTACT_NAME) {
            setComponentText(docComponentView, info.getBusinessContactName());
        } else if (component == BUSINESS_COUNTRY) {
            setComponentText(docComponentView, info.getBusinessCountry());
        } else if (component == BUSINESS_NAME) {
            setComponentText(docComponentView, info.getBusinessName());
        } else if (component == BUSINESS_PHONE) {
            setComponentText(docComponentView, info.getBusinessPhone());
        } else if (component == CUSTOMER_ADDRESS) {
            setComponentText(docComponentView, info.getCustomerAddress());
        } else if (component == CUSTOMER_CONTACT_NAME) {
            setComponentText(docComponentView, info.getCustomerContactName());
        } else if (component == CUSTOMER_COUNTRY) {
            setComponentText(docComponentView, info.getCustomerCountry());
        } else if (component == CUSTOMER_NAME) {
            setComponentText(docComponentView, info.getCustomerName());
        } else if (component == CUSTOMER_PHONE) {
            setComponentText(docComponentView, info.getCustomerPhone());
        }
    }

    private static void setComponentText(DocComponentView docComponent, String text) {
        docComponent.setComponentValue(text);
    }

}
