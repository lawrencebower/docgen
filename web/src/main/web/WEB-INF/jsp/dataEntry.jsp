<%@ taglib prefix="sf" uri="http://www.springframework.org/tags/form" %>
<%@ include file="/WEB-INF/jsp/include.jsp" %>


<script type="text/javascript">
    function setBlankTarget() {
        document.form.target = "_blank";
    }
    function setSelfTarget() {
        document.form.target = "_self";
    }
</script>

<jsp:useBean id="sessionData" scope="session" type="org.lawrencebower.docgen.web.model.SessionData"/>
<jsp:useBean id="dataEntryController" scope="session"
             type="org.lawrencebower.docgen.web.controller.DataEntryController"/>
<c:set var="fieldSeperator" value="~" scope="application"/>

<s:url var="toggleAutomapped" value="/dataEntry/toggleAutomapped"/>

<a href="${toggleAutomapped}">
    show/hide automapped
</a>

<form method="post"
      name="form"
      action="/docgen/dataEntry/setFields">

    <table>
        <c:forEach var="field"
                   items="${dataEntryController.docComponentViews}"
                   varStatus="fieldIndex">
            <tr>
                <td>
                    <label for="${field.name}">
                        <c:out value="${field.name}"/>
                    </label>
                </td>
                <td>
                    <c:if test="${field.text }">
                        <input name="${field.name}"
                               value="${field.componentValue}"
                               id="${field.name}"/>
                    </c:if>
                    <c:if test="${field.textArea}">
                        <textarea cols="30"
                                  rows="8"
                                  name="${field.name}"
                                  id="${field.name}"><c:out value="${field.componentValue}"/></textarea>
                    </c:if>
                </td>
            </tr>
        </c:forEach>
    </table>
    <input name="full" type="submit" value="full" onmousedown="setBlankTarget()"/>
    <input name="partial" type="submit" value="partial" onmousedown="setSelfTarget()"/>
</form>
