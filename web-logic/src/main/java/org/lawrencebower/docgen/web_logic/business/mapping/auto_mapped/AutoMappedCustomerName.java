package org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped;

import org.lawrencebower.docgen.web_logic.view.document.component.DocComponentView;

public class AutoMappedCustomerName extends AutoMappedComponent {

    public AutoMappedCustomerName() {
        name = CUSTOMER_NAME;
    }

    @Override
    public void mapComponent(DocComponentView docComponentView,
                             AutoMappedComponentInfo info) {

        String value = info.getCustomerName();
        setComponentValueIfMatch(docComponentView, value);
    }
}
