package org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped.component.business;

import org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped.AutoMappedFields;
import org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped.component.AbstractAutoMappedComponent;
import org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped.component.AutoMappedComponentInfo;
import org.lawrencebower.docgen.web_logic.view.document.component.DocComponentView;

public class AutoMappedBusinessNameAndAddress extends AbstractAutoMappedComponent {

    public AutoMappedBusinessNameAndAddress() {
        name = AutoMappedFields.BUSINESS_NAME_AND_ADDRESS;
    }

    @Override
    public void mapComponent(DocComponentView docComponentView,
                             AutoMappedComponentInfo info) {

        String businessName = info.getBusinessName();
        String businessAddress = info.getBusinessAddress();
        String value = businessName + "\n" + businessAddress;
        setComponentValueIfMatch(docComponentView, value);
    }
}
