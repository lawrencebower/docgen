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
<c:set var="fieldSeperator" value="~" scope="application"/>

<form method="post"
      name="form"
      action="/docgen/dataEntry/setFields">

    <c:forEach var="document"
               items="${sessionData.documents}"
               varStatus="docIndex">

        <br/><c:out value="${document.name}"/><br/><br/>

        <table>
            <c:forEach var="field"
                       items="${document.componentViews}"
                       varStatus="fieldIndex">
                <tr>
                    <td>
                        <label for="${field.name}">
                            <c:out value="${field.name}"/>
                        </label>
                    </td>
                    <td>
                        <c:if test="${field.textComponent}">
                            <input name="${document.name}${fieldSeperator}${field.name}"
                                   value="${field.value}"
                                   id="${field.name}"/>
                        </c:if>
                        <c:if test="${field.textAreaComponent}">
                            <textarea cols="50"
                                      rows="8"
                                      name="${document.name}${fieldSeperator}${field.name}"
                                      id="${field.name}"><c:out value="${field.value}"/></textarea>
                        </c:if>
                    </td>
                </tr>
            </c:forEach>
        </table>
    </c:forEach>
    <input name="full" type="submit" value="full" onmousedown="setBlankTarget()"/>
    <input name="partial" type="submit" value="partial" onmousedown="setSelfTarget()"/>
</form>
