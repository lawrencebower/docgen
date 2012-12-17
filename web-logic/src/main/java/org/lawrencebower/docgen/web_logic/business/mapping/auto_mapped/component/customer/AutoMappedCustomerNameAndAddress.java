package org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped.component.customer;

import org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped.AutoMappedFields;
import org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped.component.AbstractAutoMappedComponent;
import org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped.component.AutoMappedComponentInfo;
import org.lawrencebower.docgen.web_logic.view.document.component.DocComponentView;

public class AutoMappedCustomerNameAndAddress extends AbstractAutoMappedComponent {

    public AutoMappedCustomerNameAndAddress() {
        name = AutoMappedFields.CUSTOMER_NAME_AND_ADDRESS;
    }

    @Override
    public void mapComponent(DocComponentView docComponentView,
                             AutoMappedComponentInfo info) {

        String customerName = info.getCustomerContactName();
        String customerAddress = info.getCustomerAddress();
        String value = customerName + "\n" + customerAddress;
        setComponentValueIfMatch(docComponentView, value);
    }
}
