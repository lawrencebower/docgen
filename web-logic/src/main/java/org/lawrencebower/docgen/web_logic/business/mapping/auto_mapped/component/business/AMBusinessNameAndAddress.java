package org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped.component.business;

import org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped.AutoMappedField;
import org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped.component.AbstractAMComponent;
import org.lawrencebower.docgen.web_logic.business_def.mapping.auto_mapped.component.AMComponentInfo;
import org.lawrencebower.docgen.web_logic.view.document.component.DocComponentView;

public class AMBusinessNameAndAddress extends AbstractAMComponent {

    public AMBusinessNameAndAddress() {
        name = AutoMappedField.BUSINESS_NAME_AND_ADDRESS;
    }

    @Override
    public void mapComponent(DocComponentView docComponentView,
                             AMComponentInfo info) {

        String businessName = info.getBusinessName();
        String businessAddress = info.getBusinessAddress();
        String value = businessName + "\n" + businessAddress;
        setComponentValueIfMatch(docComponentView, value);
    }
}
