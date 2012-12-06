package org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped;

import org.lawrencebower.docgen.web_logic.view.document.component.DocComponentView;

public class AutoMappedVendorName extends AutoMappedComponent {

    public static final String VENDOR_NAME = "AUTO_MAPPED_VENDOR_NAME";

    public AutoMappedVendorName() {
        name = VENDOR_NAME;
    }

    @Override
    public void mapComponent(DocComponentView docComponentView,
                             AutoMappedComponentInfo info) {

        String value = info.getVendorName();
        setComponentValueIfMatch(docComponentView, value);
    }
}
