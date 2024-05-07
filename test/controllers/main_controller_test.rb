require "test_helper"

class MainControllerTest < ActionDispatch::IntegrationTest
  test "should get new" do
    get main_new_url
    assert_response :success
  end
end
