package org.lawrencebower.docgen.web.controller.data_entry;

import org.apache.log4j.Logger;
import org.lawrencebower.docgen.web.model.SessionData;
import org.lawrencebower.docgen.web_model.view.document.binding.DataEntryBindBean;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Scope;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.RequestMapping;

@Controller
@Scope("session")
public class ToggleAutomappedController {

    static Logger logger = Logger.getLogger(ToggleAutomappedController.class);

    private SessionData sessionData;

    @Autowired
    public void setSessionData(SessionData sessionData) {
        this.sessionData = sessionData;
    }

    @RequestMapping("/dataEntry/toggleAutomapped")
    public String toggleShowAutomappedFields(Model pageModel) {

        boolean currentValue = sessionData.isShowAutoMappedFields();
        sessionData.setShowAutoMappedFields(!currentValue);
        pageModel.addAttribute("dataEntryBean", new DataEntryBindBean());

        return "dataEntry";
    }
}
