package org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped.component.business;

import org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped.AutoMappedField;
import org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped.component.AbstractAMComponent;
import org.lawrencebower.docgen.web_logic.view.document.component.AMComponentInfo;
import org.lawrencebower.docgen.web_logic.view.document.component.DocComponentView;

public class AMBusinessPhone extends AbstractAMComponent {

    public AMBusinessPhone() {
        name = AutoMappedField.BUSINESS_PHONE;
    }

    @Override
    public void mapComponent(DocComponentView docComponentView,
                             AMComponentInfo info) {

        String value = info.getBusinessPhone();
        setComponentValueIfMatch(docComponentView, value);
    }
}
