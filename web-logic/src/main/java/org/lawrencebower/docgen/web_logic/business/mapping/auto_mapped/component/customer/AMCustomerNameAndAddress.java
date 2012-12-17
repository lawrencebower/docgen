package org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped.component.customer;

import org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped.AutoMappedFields;
import org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped.component.AMComponentInfo;
import org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped.component.AbstractAMComponent;
import org.lawrencebower.docgen.web_logic.view.document.component.DocComponentView;

public class AMCustomerNameAndAddress extends AbstractAMComponent {

    public AMCustomerNameAndAddress() {
        name = AutoMappedFields.CUSTOMER_NAME_AND_ADDRESS;
    }

    @Override
    public void mapComponent(DocComponentView docComponentView,
                             AMComponentInfo info) {

        String customerName = info.getCustomerContactName();
        String customerAddress = info.getCustomerAddress();
        String value = customerName + "\n" + customerAddress;
        setComponentValueIfMatch(docComponentView, value);
    }
}
