package org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped;

import org.lawrencebower.docgen.web_logic.view.document.component.DocComponentView;

public class AutoMappedCustomerAddress extends AutoMappedComponent {

    public static final String CUSTOMER_ADDRESS = "AUTO_MAPPED_CUSTOMER_ADDRESS";

    public AutoMappedCustomerAddress() {
        name = CUSTOMER_ADDRESS;
    }

    @Override
    public void mapComponent(DocComponentView docComponentView,
                             AutoMappedComponentInfo info) {

        String value = info.getCustomerAddress();
        setComponentValueIfMatch(docComponentView, value);
    }
}
