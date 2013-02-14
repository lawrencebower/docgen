package org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped.component.business;

import org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped.AutoMappedField;
import org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped.component.AbstractAMComponent;
import org.lawrencebower.docgen.web_model.business_def.mapping.auto_mapped.component.AMComponentInfo;
import org.lawrencebower.docgen.web_model.view.document.component.DocComponentView;

public class AMBusinessNameAndAddress extends AbstractAMComponent {

    public AMBusinessNameAndAddress() {
        name = AutoMappedField.BUSINESS_NAME_AND_ADDRESS;
    }

    @Override
    public void mapComponent(DocComponentView docComponentView,
                             AMComponentInfo info) {

        String businessContact = info.getBusinessContactName();
        String businessBusiness = info.getBusinessName();
        String businessAddress = info.getBusinessAddress();
        String businessCountry = info.getBusinessCountry();
        String value = businessContact + "\n" +
                       businessBusiness + "\n" +
                       businessAddress + "\n" +
                       businessCountry;
        setComponentValueIfMatch(docComponentView, value);
    }
}
