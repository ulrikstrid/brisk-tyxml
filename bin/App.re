open Library.React;

let make = () => {
  <div className="max-w-md rounded overflow-hidden shadow-lg">
    <img className="w-full" src="/static/sunset.jpg" />
    <div className="px-6 py-4">
      <div className="font-bold text-xl mb-2">
        <span text="The Coldest Sunset" />
      </div>
      <span
        className="text-gray-700 text-base"
        text="Lorem ipsum dolor sit amet, consectetur adipisicing elit. Voluptatibus quia, nulla! Maiores et perferendis eaque, exercitationem praesentium nihil."
      />
    </div>
    <div className="px-6 py-4">
      <span
        className="inline-block bg-gray-200 rounded-full px-3 py-1 text-sm font-semibold text-gray-700 mr-2 cursor-pointer select-none"
        text="#photography"
      />
      <span
        className="inline-block bg-gray-200 rounded-full px-3 py-1 text-sm font-semibold text-gray-700 mr-2 cursor-pointer select-none"
        text="#travel"
      />
      <span
        className="inline-block bg-gray-200 rounded-full px-3 py-1 text-sm font-semibold text-gray-700 cursor-pointer select-none"
        text="#winter "
      />
    </div>
  </div>;
};
