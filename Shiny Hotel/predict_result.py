
import joblib
def show_rating():
  # load the model from disk
  dt_saved = joblib.load('others/test_zomato_classification_model.pkl')
  # dt_saved = load_model('others/test_zomato_classification_model')
  df_test = pd.read_csv(r"others/data_input.csv")
  ans = dt_saved.predict(df_test)
  ans = ans[0]
  return ans
  
