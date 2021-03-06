package org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped.component.business;

import org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped.AutoMappedField;
import org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped.component.AbstractAMComponent;
import org.lawrencebower.docgen.web_model.business_def.mapping.auto_mapped.component.AMComponentInfo;
import org.lawrencebower.docgen.web_model.view.document.component.DocComponentView;

public class AMBusinessContactName extends AbstractAMComponent {

    public AMBusinessContactName() {
        name = AutoMappedField.BUSINESS_CONTACT_NAME;
    }

    @Override
    public void mapComponent(DocComponentView docComponentView,
                             AMComponentInfo info) {

        String value = info.getBusinessContactName();
        setComponentValueIfMatch(docComponentView, value);
    }
}
