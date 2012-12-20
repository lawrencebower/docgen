package org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped.component.customer;

import org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped.AutoMappedField;
import org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped.component.AMComponentInfo;
import org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped.component.AbstractAMComponent;
import org.lawrencebower.docgen.web_logic.view.document.component.DocComponentView;

public class AMCustomerPhone extends AbstractAMComponent {

    public AMCustomerPhone() {
        name = AutoMappedField.CUSTOMER_PHONE;
    }

    @Override
    public void mapComponent(DocComponentView docComponentView,
                             AMComponentInfo info) {

        String value = info.getCustomerPhone();
        setComponentValueIfMatch(docComponentView, value);
    }
}