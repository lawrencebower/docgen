package org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped.component.date;

import org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped.AutoMappedField;
import org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped.component.AbstractAMComponent;
import org.lawrencebower.docgen.web_model.business_def.mapping.auto_mapped.component.AMComponentInfo;
import org.lawrencebower.docgen.web_model.view.document.component.DocComponentView;

import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;

public class AMDate extends AbstractAMComponent {

    private DateFormat dateFormat = new SimpleDateFormat("dd MMMM yyyy");

    public AMDate() {
        name = AutoMappedField.DATE;
    }

    @Override
    public void mapComponent(DocComponentView docComponentView,
                             AMComponentInfo info) {

        Calendar instance = Calendar.getInstance();
        Date time = instance.getTime();
        String value = dateFormat.format(time);
        setComponentValueIfMatch(docComponentView, value);
    }
}
