package org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped.component.customer;

import org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped.AutoMappedField;
import org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped.component.AbstractAMComponent;
import org.lawrencebower.docgen.web_model.business_def.mapping.auto_mapped.component.AMComponentInfo;
import org.lawrencebower.docgen.web_model.view.document.component.DocComponentView;

public class AMCustomerNameAndAddress extends AbstractAMComponent {

    public AMCustomerNameAndAddress() {
        name = AutoMappedField.CUSTOMER_NAME_AND_ADDRESS;
    }

    @Override
    public void mapComponent(DocComponentView docComponentView,
                             AMComponentInfo info) {

        String customerContact = info.getCustomerContactName();
        String customerBusiness = info.getBusinessName();
        String customerAddress = info.getCustomerAddress();
        String customerCountry = info.getCustomerCountry();
        String value = customerContact + "\n" +
                       customerBusiness + "\n" +
                       customerAddress + "\n" +
                       customerCountry;
        setComponentValueIfMatch(docComponentView, value);
    }
}
