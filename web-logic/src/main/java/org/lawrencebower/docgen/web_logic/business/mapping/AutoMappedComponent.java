package org.lawrencebower.docgen.web_logic.business.mapping;

import org.lawrencebower.docgen.core.exception.DocGenException;
import org.lawrencebower.docgen.web_logic.view.document.component.DocComponentView;

public enum AutoMappedComponent {

    BUSINESS_NAME("AUTO_MAPPED_BUSINESS_NAME"),
    BUSINESS_CONTACT_NAME("AUTO_MAPPED_BUSINESS_CONTACT_NAME"),
    BUSINESS_ADDRESS("AUTO_MAPPED_BUSINESS_ADDRESS"),
    BUSINESS_PHONE("AUTO_MAPPED_BUSINESS_PHONE"),
    BUSINESS_COUNTRY("AUTO_MAPPED_BUSINESS_COUNTRY"),

    CUSTOMER_NAME("AUTO_MAPPED_CUSTOMER_NAME"),
    CUSTOMER_CONTACT_NAME("AUTO_MAPPED_CUSTOMER_CONTACT_NAME"),
    CUSTOMER_ADDRESS("AUTO_MAPPED_CUSTOMER_ADDRESS"),
    CUSTOMER_PHONE("AUTO_MAPPED_CUSTOMER_PHONE"),
    CUSTOMER_COUNTRY("AUTO_MAPPED_CUSTOMER_COUNTRY"),

    VENDOR_NAME("AUTO_MAPPED_VENDOR_NAME"),
    VENDOR_CONTACT_NAME("AUTO_MAPPED_VENDOR_CONTACT_NAME"),
    VENDOR_ADDRESS("AUTO_MAPPED_VENDOR_ADDRESS"),
    VENDOR_PHONE("AUTO_MAPPED_VENDOR_PHONE"),
    VENDOR_TAX_ID("AUTO_MAPPED_VENDOR_TAX_ID"),
    VENDOR_COUNTRY("AUTO_MAPPED_VENDOR_COUNTRY"),
    VENDOR_EMAIL("AUTO_MAPPED_VENDOR_EMAIL");

    private String name;

    AutoMappedComponent(String name) {
        this.name = name;
    }

    public String getName() {
        return name;
    }

    public static boolean containsName(String name) {

        boolean containsName = false;

        for (AutoMappedComponent autoMappedField : AutoMappedComponent.values()) {
            String autoMappedName = autoMappedField.getName();
            if (autoMappedName.equals(name)) {
                containsName = true;
                break;
            }
        }

        return containsName;
    }

    public static AutoMappedComponent getByFieldName(String name) {

        for (AutoMappedComponent autoMappedField : AutoMappedComponent.values()) {
            String autoMappedName = autoMappedField.getName();
            if (autoMappedName.equals(name)) {
                return autoMappedField;
            }
        }

        throw new DocGenException("enum productInjectionField not found with name " + name);
    }

    public static void mapComponent(DocComponentView docComponentView,
                                    AutoMappedComponentInfo info) {

        String componentName = docComponentView.getName();

        AutoMappedComponent field = getByFieldName(componentName);

        if (field == VENDOR_ADDRESS) {
            setComponentText(docComponentView, info.getVendorAddress());
        } else if (field == VENDOR_CONTACT_NAME) {
            setComponentText(docComponentView, info.getVendorContactName());
        } else if (field == VENDOR_COUNTRY) {
            setComponentText(docComponentView, info.getVendorCountry());
        } else if (field == VENDOR_NAME) {
            setComponentText(docComponentView, info.getVendorName());
        } else if (field == VENDOR_PHONE) {
            setComponentText(docComponentView, info.getVendorPhone());
        } else if (field == VENDOR_EMAIL) {
            setComponentText(docComponentView, info.getVendorEmail());
        } else if (field == VENDOR_TAX_ID) {
            setComponentText(docComponentView, info.getVendorTaxId());
        } else if (field == BUSINESS_ADDRESS) {
            setComponentText(docComponentView, info.getBusinessAddress());
        } else if (field == BUSINESS_CONTACT_NAME) {
            setComponentText(docComponentView, info.getBusinessContactName());
        } else if (field == BUSINESS_COUNTRY) {
            setComponentText(docComponentView, info.getBusinessCountry());
        } else if (field == BUSINESS_NAME) {
            setComponentText(docComponentView, info.getBusinessName());
        } else if (field == BUSINESS_PHONE) {
            setComponentText(docComponentView, info.getBusinessPhone());
        } else if (field == CUSTOMER_ADDRESS) {
            setComponentText(docComponentView, info.getCustomerAddress());
        } else if (field == CUSTOMER_CONTACT_NAME) {
            setComponentText(docComponentView, info.getCustomerContactName());
        } else if (field == CUSTOMER_COUNTRY) {
            setComponentText(docComponentView, info.getCustomerCountry());
        } else if (field == CUSTOMER_NAME) {
            setComponentText(docComponentView, info.getCustomerName());
        } else if (field == CUSTOMER_PHONE) {
            setComponentText(docComponentView, info.getCustomerPhone());
        }
    }

    private static void setComponentText(DocComponentView docComponent, String text) {
        docComponent.setComponentValue(text);
    }

}
