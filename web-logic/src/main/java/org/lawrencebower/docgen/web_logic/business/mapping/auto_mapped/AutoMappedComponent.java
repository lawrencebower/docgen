package org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped;

import org.lawrencebower.docgen.web_logic.view.document.component.DocComponentView;

public abstract class AutoMappedComponent {

    public static final String BUSINESS_ADDRESS = "AUTO_MAPPED_BUSINESS_ADDRESS";
    public static final String BUSINESS_CONTACT_NAME = "AUTO_MAPPED_BUSINESS_CONTACT_NAME";
    public static final String BUSINESS_COUNTRY = "AUTO_MAPPED_BUSINESS_COUNTRY";
    public static final String BUSINESS_NAME = "AUTO_MAPPED_BUSINESS_NAME";
    public static final String BUSINESS_NAME_AND_ADDRESS = "AUTO_MAPPED_BUSINESS_NAME_AND_ADDRESS";
    public static final String BUSINESS_PHONE = "AUTO_MAPPED_BUSINESS_PHONE";
    public static final String CUSTOMER_ADDRESS = "AUTO_MAPPED_CUSTOMER_ADDRESS";
    public static final String CUSTOMER_CONTACT_NAME = "AUTO_MAPPED_CUSTOMER_CONTACT_NAME";
    public static final String CUSTOMER_COUNTRY = "AUTO_MAPPED_CUSTOMER_COUNTRY";
    public static final String CUSTOMER_NAME = "AUTO_MAPPED_CUSTOMER_NAME";
    public static final String CUSTOMER_NAME_AND_ADDRESS = "AUTO_MAPPED_CUSTOMER_NAME_AND_ADDRESS";
    public static final String CUSTOMER_PHONE = "AUTO_MAPPED_CUSTOMER_PHONE";
    public static final String VENDOR_ADDRESS = "AUTO_MAPPED_VENDOR_ADDRESS";
    public static final String VENDOR_CONTACT_NAME = "AUTO_MAPPED_VENDOR_CONTACT_NAME";
    public static final String VENDOR_COUNTRY = "AUTO_MAPPED_VENDOR_COUNTRY";
    public static final String VENDOR_EMAIL = "AUTO_MAPPED_VENDOR_EMAIL";
    public static final String VENDOR_NAME = "AUTO_MAPPED_VENDOR_NAME";
    public static final String VENDOR_PHONE = "AUTO_MAPPED_VENDOR_PHONE";
    public static final String VENDOR_TAX_ID = "AUTO_MAPPED_VENDOR_TAX_ID";
    public static final String VENDOR_NAME_AND_ADDRESS = "AUTO_MAPPED_VENDOR_NAME_AND_ADDRESS";

    protected String name;

    public String getName() {
        return name;
    }

    public abstract void mapComponent(DocComponentView docComponentView,
                                      AutoMappedComponentInfo info);

    protected boolean setComponentValueIfMatch(DocComponentView docComponent,
                                               String value) {

        String componentName = docComponent.getName();

        boolean componentMatched = false;

        if (matchesName(componentName)) {
            setComponentText(docComponent, value);
            componentMatched = true;
        }

        return componentMatched;
    }

    public boolean matchesName(String name) {
        return this.name.equals(name);
    }

    private void setComponentText(DocComponentView docComponent, String text) {
        docComponent.setComponentValue(text);
    }

}
