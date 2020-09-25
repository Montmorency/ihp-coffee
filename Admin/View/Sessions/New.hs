module Admin.View.Sessions.New where
import Admin.View.Prelude
import IHP.AuthSupport.View.Sessions.New hiding (NewView)

data NewView admin = NewView { admin :: admin } deriving (Typeable)

instance View (NewView Admin) ViewContext where
    html NewView { .. } = [hsx|
        <div class="h-100" id="sessions-new">
            <div class="d-flex align-items-center">
                <div class="w-100">
                    <div style="max-width: 400px" class="mx-auto mb-5">
                        {renderFlashMessages}
                        <h5>Please login:</h5>
                        {renderForm admin}
                    </div>
                </div>
            </div>
        </div>
    |]

renderForm :: Admin -> Html
renderForm admin = [hsx|
    <form method="POST" action={CreateSessionAction}>
        <div class="form-group">
            <input name="email" value={get #email admin} type="email" class="form-control" placeholder="E-Mail"/>
        </div>
        <div class="form-group">
            <input name="password" type="password" class="form-control" placeholder="Password"/>
        </div>
        <button type="submit" class="btn btn-primary btn-block">Login</button>
    </form>
|]
