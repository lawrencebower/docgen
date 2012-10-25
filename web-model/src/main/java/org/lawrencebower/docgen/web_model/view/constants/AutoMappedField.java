package org.lawrencebower.docgen.web_model.view.constants;

import org.lawrencebower.docgen.core.exception.DocGenException;

public enum AutoMappedField {

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

    private AutoMappedField(String name) {
        this.name = name;
    }

    public String getName() {
        return name;
    }

    public static boolean containsName(String name) {

        for (AutoMappedField autoMappedField : AutoMappedField.values()) {
            if (autoMappedField.getName().equals(name)) {
                return true;
            }
        }

        return false;
    }

    public static AutoMappedField getByFieldName(String name) {
        for (AutoMappedField autoMappedField : AutoMappedField.values()) {
            if (autoMappedField.getName().equals(name)) {
                return autoMappedField;
            }
        }
        throw new DocGenException("enum AutoMappedField not found with name " + name);
    }
}
