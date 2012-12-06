package org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped;

import org.lawrencebower.docgen.web_logic.view.document.component.DocComponentView;

public class AutoMappedCustomerPhone extends AutoMappedComponent {

    public static final String CUSTOMER_PHONE = "AUTO_MAPPED_CUSTOMER_PHONE";

    public AutoMappedCustomerPhone() {
        name = CUSTOMER_PHONE;
    }

    @Override
    public void mapComponent(DocComponentView docComponentView,
                             AutoMappedComponentInfo info) {

        String value = info.getCustomerPhone();
        setComponentValueIfMatch(docComponentView, value);
    }
}
