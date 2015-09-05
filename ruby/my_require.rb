# WIP not done yet


$EOIN_LOADED_FEATURES = []

# places where ruby features (.rb and .c) lives on my system
$EOIN_LOAD_PATH = []

# return true if we load something, false otherwise
def eoin_require(feature_name)
  if pathish?(feature_name)
    full_path = File.join(Dir.pwd, feature_name)
  else
    full_path = $EOIN_LOAD_PATH.first do |path|
      File.exist?(File.join(path, feature_name))
    end
  end

  # TODO: throw a load error if we cannot find the feature on disk

  # return false if the feature was already loaded
  return false if $EOIN_LOADED_FEATURES.include?(full_path)

  eval File.read(full_path)
  $EOIN_LOADED_FEATURES << full_path
  true
end

# run at boot:
def add_stdlib_to_loadpath
  # ...
end
