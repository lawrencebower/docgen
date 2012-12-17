package org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped.component.customer;

import org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped.AutoMappedFields;
import org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped.component.AMComponentInfo;
import org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped.component.AbstractAMComponent;
import org.lawrencebower.docgen.web_logic.view.document.component.DocComponentView;

public class AMCustomerAddress extends AbstractAMComponent {

    public AMCustomerAddress() {
        name = AutoMappedFields.CUSTOMER_ADDRESS;
    }

    @Override
    public void mapComponent(DocComponentView docComponentView,
                             AMComponentInfo info) {

        String value = info.getCustomerAddress();
        setComponentValueIfMatch(docComponentView, value);
    }
}
