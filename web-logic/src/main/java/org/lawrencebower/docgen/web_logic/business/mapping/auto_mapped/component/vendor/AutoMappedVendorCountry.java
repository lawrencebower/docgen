package org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped.component.vendor;

import org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped.AutoMappedFields;
import org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped.component.AbstractAutoMappedComponent;
import org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped.component.AutoMappedComponentInfo;
import org.lawrencebower.docgen.web_logic.view.document.component.DocComponentView;

public class AutoMappedVendorCountry extends AbstractAutoMappedComponent {

    public AutoMappedVendorCountry() {
        name = AutoMappedFields.VENDOR_COUNTRY;
    }

    @Override
    public void mapComponent(DocComponentView docComponentView,
                             AutoMappedComponentInfo info) {

        String value = info.getVendorCountry();
        setComponentValueIfMatch(docComponentView, value);
    }
}