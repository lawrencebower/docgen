package org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped.component.vendor;

import org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped.AutoMappedFields;
import org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped.component.AbstractAutoMappedComponent;
import org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped.component.AutoMappedComponentInfo;
import org.lawrencebower.docgen.web_logic.view.document.component.DocComponentView;

public class AutoMappedVendorEmail extends AbstractAutoMappedComponent {

    public AutoMappedVendorEmail() {
        name = AutoMappedFields.VENDOR_EMAIL;
    }

    @Override
    public void mapComponent(DocComponentView docComponentView,
                             AutoMappedComponentInfo info) {

        String value = info.getVendorEmail();
        setComponentValueIfMatch(docComponentView, value);
    }
}
