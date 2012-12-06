package org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped;

import org.lawrencebower.docgen.web_logic.view.document.component.DocComponentView;

public class AutoMappedCustomerContactName extends AutoMappedComponent {

    public AutoMappedCustomerContactName() {
        name = CUSTOMER_CONTACT_NAME;
    }

    @Override
    public void mapComponent(DocComponentView docComponentView,
                             AutoMappedComponentInfo info) {

        String value = info.getCustomerContactName();
        setComponentValueIfMatch(docComponentView, value);
    }
}