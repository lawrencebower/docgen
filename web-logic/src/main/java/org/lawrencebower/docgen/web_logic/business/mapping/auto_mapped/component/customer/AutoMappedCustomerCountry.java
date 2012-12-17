package org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped.component.customer;

import org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped.AutoMappedFields;
import org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped.component.AbstractAutoMappedComponent;
import org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped.component.AutoMappedComponentInfo;
import org.lawrencebower.docgen.web_logic.view.document.component.DocComponentView;

public class AutoMappedCustomerCountry extends AbstractAutoMappedComponent {

    public AutoMappedCustomerCountry() {
        name = AutoMappedFields.CUSTOMER_COUNTRY;
    }

    @Override
    public void mapComponent(DocComponentView docComponentView,
                             AutoMappedComponentInfo info) {

        String value = info.getCustomerCountry();
        setComponentValueIfMatch(docComponentView, value);
    }
}
