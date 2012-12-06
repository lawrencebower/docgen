package org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped;

import org.lawrencebower.docgen.web_logic.view.document.component.DocComponentView;

public class AutoMappedCustomerCountry extends AutoMappedComponent {

    public AutoMappedCustomerCountry() {
        name = CUSTOMER_COUNTRY;
    }

    @Override
    public void mapComponent(DocComponentView docComponentView,
                             AutoMappedComponentInfo info) {

        String value = info.getCustomerCountry();
        setComponentValueIfMatch(docComponentView, value);
    }
}
