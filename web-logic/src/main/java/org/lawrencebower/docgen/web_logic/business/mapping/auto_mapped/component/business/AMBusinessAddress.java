package org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped.component.business;

import org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped.AutoMappedFields;
import org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped.component.AMComponentInfo;
import org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped.component.AbstractAMComponent;
import org.lawrencebower.docgen.web_logic.view.document.component.DocComponentView;

public class AMBusinessAddress extends AbstractAMComponent {

    public AMBusinessAddress() {
        name = AutoMappedFields.BUSINESS_ADDRESS;
    }

    @Override
    public void mapComponent(DocComponentView docComponentView,
                             AMComponentInfo info) {

        String value = info.getBusinessAddress();
        setComponentValueIfMatch(docComponentView, value);
    }
}
