package org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped.component.business;

import org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped.AutoMappedFields;
import org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped.component.AMComponentInfo;
import org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped.component.AbstractAMComponent;
import org.lawrencebower.docgen.web_logic.view.document.component.DocComponentView;

public class AMBusinessContactName extends AbstractAMComponent {

    public AMBusinessContactName() {
        name = AutoMappedFields.BUSINESS_CONTACT_NAME;
    }

    @Override
    public void mapComponent(DocComponentView docComponentView,
                             AMComponentInfo info) {

        String value = info.getBusinessContactName();
        setComponentValueIfMatch(docComponentView, value);
    }
}
