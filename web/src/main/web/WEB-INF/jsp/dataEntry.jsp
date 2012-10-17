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

<div>
    <ol>

        <form method="post"
              name="form"
              action="/docgen/dataEntry/setFields">

            <fieldset>

                <c:forEach var="document"
                           items="${sessionData.documents}"
                           varStatus="docIndex">

                    <br/><c:out value="${document.name}"/><br/><br/>

                    <c:forEach var="field"
                               items="${document.editableFields}"
                               varStatus="fieldIndex">

                        <c:if test="${field.textComponent}">
                            <label>
                                <c:out value="${field.name}"/>
                                <input name="${document.name}${fieldSeperator}${field.name}"
                                       value="${field.value}"/>
                            </label>
                            <br/>
                        </c:if>
                    </c:forEach>
                </c:forEach>
                <input name="full" type="submit" value="full" onmousedown="setBlankTarget()"/>
                <input name="partial" type="submit" value="partial" onmousedown="setSelfTarget()"/>
            </fieldset>
        </form>
    </ol>
</div>