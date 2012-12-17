package org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped.component.business;

import org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped.AutoMappedFields;
import org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped.component.AbstractAutoMappedComponent;
import org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped.component.AutoMappedComponentInfo;
import org.lawrencebower.docgen.web_logic.view.document.component.DocComponentView;

public class AutoMappedBusinessName extends AbstractAutoMappedComponent {

    public AutoMappedBusinessName() {
        name = AutoMappedFields.BUSINESS_NAME;
    }

    @Override
    public void mapComponent(DocComponentView docComponentView,
                             AutoMappedComponentInfo info) {

        String value = info.getBusinessName();
        setComponentValueIfMatch(docComponentView, value);
    }
}
