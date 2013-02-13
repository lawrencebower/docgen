package org.lawrencebower.docgen.web.controller.general;

import org.lawrencebower.docgen.web.model.SessionData;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Scope;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.RequestMapping;

@Controller
@Scope("session")
public class ClearSessionController {

    @Autowired
    SessionData sessionData;

    @RequestMapping("/clearSession")
    public String clearSession(Model model){

        sessionData.clear();

        return "redirect:home";
    }

}
